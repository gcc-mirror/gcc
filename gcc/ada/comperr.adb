------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O M P E R R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.3.10.2 $
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines called when a fatal internal compiler
--  error is detected. Calls to these routines cause termination of the
--  current compilation with appropriate error output.

with Atree;    use Atree;
with Debug;    use Debug;
with Errout;   use Errout;
with Fname;    use Fname;
with Gnatvsn;  use Gnatvsn;
with Lib;      use Lib;
with Namet;    use Namet;
with Osint;    use Osint;
with Output;   use Output;
with Sinput;   use Sinput;
with Sprint;   use Sprint;
with Sdefault; use Sdefault;
with Treepr;   use Treepr;
with Types;    use Types;

with Ada.Exceptions; use Ada.Exceptions;

with System.Soft_Links; use System.Soft_Links;

package body Comperr is

   ----------------
   -- Local Data --
   ----------------

   Abort_In_Progress : Boolean := False;
   --  Used to prevent runaway recursion if something segfaults
   --  while processing a previous abort.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Repeat_Char (Char : Character; Col : Nat; After : Character);
   --  Output Char until current column is at or past Col, and then output
   --  the character given by After (if column is already past Col on entry,
   --  then the effect is simply to output the After character).

   --------------------
   -- Compiler_Abort --
   --------------------

   procedure Compiler_Abort
     (X    : String;
      Code : Integer := 0)
   is
      procedure End_Line;
      --  Add blanks up to column 76, and then a final vertical bar

      procedure End_Line is
      begin
         Repeat_Char (' ', 76, '|');
         Write_Eol;
      end End_Line;

   --  Start of processing for Compiler_Abort

   begin
      --  Prevent recursion through Compiler_Abort, e.g. via SIGSEGV.

      if Abort_In_Progress then
         Exit_Program (E_Abort);
      end if;

      Abort_In_Progress := True;

      --  If errors have already occurred, then we guess that the abort may
      --  well be caused by previous errors, and we don't make too much fuss
      --  about it, since we want to let the programmer fix the errors first.

      --  Debug flag K disables this behavior (useful for debugging)

      if Errors_Detected /= 0 and then not Debug_Flag_K then
         Errout.Finalize;

         Set_Standard_Error;
         Write_Str ("compilation abandoned due to previous error");
         Write_Eol;

         Set_Standard_Output;
         Source_Dump;
         Tree_Dump;
         Exit_Program (E_Errors);

      --  Otherwise give message with details of the abort

      else
         Set_Standard_Error;

         --  Generate header for bug box

         Write_Char ('+');
         Repeat_Char ('=', 29, 'G');
         Write_Str ("NAT BUG DETECTED");
         Repeat_Char ('=', 76, '+');
         Write_Eol;

         --  Output GNAT version identification

         Write_Str ("| ");
         Write_Str (Gnat_Version_String);
         Write_Str (" (");

         --  Output target name, deleting junk final reverse slash

         if Target_Name.all (Target_Name.all'Last) = '\'
           or else Target_Name.all (Target_Name.all'Last) = '/'
         then
            Write_Str (Target_Name.all (1 .. Target_Name.all'Last - 1));
         else
            Write_Str (Target_Name.all);
         end if;

         --  Output identification of error

         Write_Str (") ");

         if X'Length + Column > 76 then
            if Code < 0 then
               Write_Str ("GCC error:");
            end if;

            End_Line;

            Write_Str ("| ");
         end if;

         if X'Length > 70 then
            declare
               Last_Blank : Integer := 70;

            begin
               for P in 40 .. 69 loop
                  if X (P) = ' ' then
                     Last_Blank := P;
                  end if;
               end loop;

               Write_Str (X (1 .. Last_Blank));
               End_Line;
               Write_Str ("|    ");
               Write_Str (X (Last_Blank + 1 .. X'Length));
            end;
         else
            Write_Str (X);
         end if;

         if Code > 0 then
            Write_Str (", Code=");
            Write_Int (Int (Code));

         elsif Code = 0 then

            --  For exception case, get exception message from the TSD. Note
            --  that it would be neater and cleaner to pass the exception
            --  message (obtained from Exception_Message) as a parameter to
            --  Compiler_Abort, but we can't do this quite yet since it would
            --  cause bootstrap path problems for 3.10 to 3.11.

            Write_Char (' ');
            Write_Str (Exception_Message (Get_Current_Excep.all.all));
         end if;

         End_Line;

         --  Output source location information

         if Sloc (Current_Error_Node) <= Standard_Location
           or else Sloc (Current_Error_Node) = No_Location
         then
            Write_Str ("| No source file position information available");
            End_Line;
         else
            Write_Str ("| Error detected at ");
            Write_Location (Sloc (Current_Error_Node));
            End_Line;
         end if;

         --  There are two cases now. If the file gnat_bug.box exists,
         --  we use the contents of this file at this point.

         declare
            Lo  : Source_Ptr;
            Hi  : Source_Ptr;
            Src : Source_Buffer_Ptr;

         begin
            Namet.Unlock;
            Name_Buffer (1 .. 12) := "gnat_bug.box";
            Name_Len := 12;
            Read_Source_File (Name_Enter, 0, Hi, Src);

            --  If we get a Src file, we use it

            if Src /= null then
               Lo := 0;

               Outer : while Lo < Hi loop
                  Write_Str ("| ");

                  Inner : loop
                     exit Inner when Src (Lo) = ASCII.CR
                       or else Src (Lo) = ASCII.LF;
                     Write_Char (Src (Lo));
                     Lo := Lo + 1;
                  end loop Inner;

                  End_Line;

                  while Lo <= Hi
                    and then (Src (Lo) = ASCII.CR
                                or else Src (Lo) = ASCII.LF)
                  loop
                     Lo := Lo + 1;
                  end loop;
               end loop Outer;

            --  Otherwise we use the standard fixed text

            else
               Write_Str
                 ("| Please submit a bug report, see" &
                  " http://gcc.gnu.org/bugs.html.");
               End_Line;

               Write_Str
                 ("| Include the entire contents of this bug " &
                  "box in the report.");
               End_Line;

               Write_Str
                 ("| Include the exact gcc or gnatmake command " &
                  "that you entered.");
               End_Line;

               Write_Str
                 ("| Also include sources listed below in gnatchop format");
               End_Line;

               Write_Str
                 ("| (concatenated together with no headers between files).");
               End_Line;

            end if;
         end;

         --  Complete output of bug box

         Write_Char ('+');
         Repeat_Char ('=', 76, '+');
         Write_Eol;

         if Debug_Flag_3 then
            Write_Eol;
            Write_Eol;
            Print_Tree_Node (Current_Error_Node);
            Write_Eol;
         end if;

         Write_Eol;

         Write_Line ("Please include these source files with error report");
         Write_Eol;

         for U in Main_Unit .. Last_Unit loop
            begin
               if not Is_Internal_File_Name
                        (File_Name (Source_Index (U)))
               then
                  Write_Name (Full_File_Name (Source_Index (U)));
                  Write_Eol;
               end if;

            --  No point in double bug box if we blow up trying to print
            --  the list of file names! Output informative msg and quit.

            exception
               when others =>
                  Write_Str ("list may be incomplete");
                  exit;
            end;
         end loop;

         Write_Eol;
         Set_Standard_Output;

         Tree_Dump;
         Source_Dump;
         raise Unrecoverable_Error;
      end if;

   end Compiler_Abort;

   -----------------
   -- Repeat_Char --
   -----------------

   procedure Repeat_Char (Char : Character; Col : Nat; After : Character) is
   begin
      while Column < Col loop
         Write_Char (Char);
      end loop;

      Write_Char (After);
   end Repeat_Char;

end Comperr;
