------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O M P E R R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by AdaCore.                         --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines called when a fatal internal compiler error
--  is detected. Calls to these routines cause termination of the current
--  compilation with appropriate error output.

with Atree;    use Atree;
with Debug;    use Debug;
with Errout;   use Errout;
with Gnatvsn;  use Gnatvsn;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Sprint;   use Sprint;
with Sdefault; use Sdefault;
with Treepr;   use Treepr;
with Types;    use Types;

with Ada.Exceptions; use Ada.Exceptions;

with System.OS_Lib;     use System.OS_Lib;
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
     (X            : String;
      Fallback_Loc : String  := "";
      From_GCC     : Boolean := False)
   is
      --  The procedures below output a "bug box" with information about
      --  the cause of the compiler abort and about the preferred method
      --  of reporting bugs. The default is a bug box appropriate for
      --  the FSF version of GNAT, but there are specializations for
      --  the GNATPRO and Public releases by AdaCore.

      XF : constant Positive := X'First;
      --  Start index, usually 1, but we won't assume this

      procedure End_Line;
      --  Add blanks up to column 76, and then a final vertical bar

      --------------
      -- End_Line --
      --------------

      procedure End_Line is
      begin
         Repeat_Char (' ', 76, '|');
         Write_Eol;
      end End_Line;

      Is_GPL_Version : constant Boolean := Gnatvsn.Build_Type = GPL;
      Is_FSF_Version : constant Boolean := Gnatvsn.Build_Type = FSF;

   --  Start of processing for Compiler_Abort

   begin
      Cancel_Special_Output;

      --  Prevent recursion through Compiler_Abort, e.g. via SIGSEGV

      if Abort_In_Progress then
         Exit_Program (E_Abort);
      end if;

      Abort_In_Progress := True;

      --  Generate a "standard" error message instead of a bug box in case
      --  of CodePeer rather than generating a bug box, friendlier.

      --  Note that the call to Error_Msg_N below sets Serious_Errors_Detected
      --  to 1, so we use the regular mechanism below in order to display a
      --  "compilation abandoned" message and exit, so we still know we have
      --  this case (and -gnatdk can still be used to get the bug box).

      if CodePeer_Mode
        and then Serious_Errors_Detected = 0
        and then not Debug_Flag_K
        and then Sloc (Current_Error_Node) > No_Location
      then
         Error_Msg_N ("cannot generate 'S'C'I'L", Current_Error_Node);
      end if;

      --  If we are in CodePeer mode, we must also delete SCIL files

      if CodePeer_Mode then
         Delete_SCIL_Files;
      end if;

      --  If any errors have already occurred, then we guess that the abort
      --  may well be caused by previous errors, and we don't make too much
      --  fuss about it, since we want to let programmer fix the errors first.

      --  Debug flag K disables this behavior (useful for debugging)

      if Serious_Errors_Detected /= 0 and then not Debug_Flag_K then
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;

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
            if From_GCC then
               Write_Str ("GCC error:");
            end if;

            End_Line;

            Write_Str ("| ");
         end if;

         if X'Length > 70 then
            declare
               Last_Blank : Integer := 70;

            begin
               for P in 39 .. 68 loop
                  if X (XF + P) = ' ' then
                     Last_Blank := P;
                  end if;
               end loop;

               Write_Str (X (XF .. XF - 1 + Last_Blank));
               End_Line;
               Write_Str ("|    ");
               Write_Str (X (XF + Last_Blank .. X'Last));
            end;
         else
            Write_Str (X);
         end if;

         if not From_GCC then

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

         if Sloc (Current_Error_Node) <= No_Location then
            if Fallback_Loc'Length > 0 then
               Write_Str ("| Error detected around ");
               Write_Str (Fallback_Loc);
            else
               Write_Str ("| No source file position information available");
            end if;

            End_Line;
         else
            Write_Str ("| Error detected at ");
            Write_Location (Sloc (Current_Error_Node));
            End_Line;
         end if;

         --  There are two cases now. If the file gnat_bug.box exists,
         --  we use the contents of this file at this point.

         declare
            FD  : File_Descriptor;
            Lo  : Source_Ptr;
            Hi  : Source_Ptr;
            Src : Source_Buffer_Ptr;

         begin
            Namet.Unlock;
            Name_Buffer (1 .. 12) := "gnat_bug.box";
            Name_Len := 12;
            Read_Source_File (Name_Enter, 0, Hi, Src, FD);

            --  If we get a Src file, we use it

            if not Null_Source_Buffer_Ptr (Src) then
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
               if Is_FSF_Version then
                  Write_Str
                    ("| Please submit a bug report; see" &
                     " https://gcc.gnu.org/bugs/ .");
                  End_Line;

               elsif Is_GPL_Version then

                  Write_Str
                    ("| Please submit a bug report by email " &
                     "to report@adacore.com.");
                  End_Line;

                  Write_Str
                    ("| GAP members can alternatively use GNAT Tracker:");
                  End_Line;

                  Write_Str
                    ("| http://www.adacore.com/ " &
                     "section 'send a report'.");
                  End_Line;

                  Write_Str
                    ("| See gnatinfo.txt for full info on procedure " &
                     "for submitting bugs.");
                  End_Line;

               else
                  Write_Str
                    ("| Please submit a bug report using GNAT Tracker:");
                  End_Line;

                  Write_Str
                    ("| http://www.adacore.com/gnattracker/ " &
                     "section 'send a report'.");
                  End_Line;

                  Write_Str
                    ("| alternatively submit a bug report by email " &
                     "to report@adacore.com,");
                  End_Line;

                  Write_Str
                    ("| including your customer number #nnn " &
                     "in the subject line.");
                  End_Line;
               end if;

               Write_Str
                 ("| Use a subject line meaningful to you" &
                  " and us to track the bug.");
               End_Line;

               Write_Str
                 ("| Include the entire contents of this bug " &
                  "box in the report.");
               End_Line;

               Write_Str
                 ("| Include the exact command that you entered.");
               End_Line;

               Write_Str
                 ("| Also include sources listed below.");
               End_Line;

               if not Is_FSF_Version then
                  Write_Str
                    ("| Use plain ASCII or MIME attachment(s).");
                  End_Line;
               end if;
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
         Write_Line ("Note that list may not be accurate in some cases, ");
         Write_Line ("so please double check that the problem can still ");
         Write_Line ("be reproduced with the set of files listed.");
         Write_Line ("Consider also -gnatd.n switch (see debug.adb).");
         Write_Eol;

         begin
            Dump_Source_File_Names;

         --  If we blow up trying to print the list of file names, just output
         --  informative msg and continue.

         exception
            when others =>
               Write_Str ("list may be incomplete");
         end;

         Write_Eol;
         Set_Standard_Output;

         Tree_Dump;
         Source_Dump;
         raise Unrecoverable_Error;
      end if;
   end Compiler_Abort;

   -----------------------
   -- Delete_SCIL_Files --
   -----------------------

   procedure Delete_SCIL_Files is
      Main      : Node_Id;
      Unit_Name : Node_Id;

      Success : Boolean;
      pragma Unreferenced (Success);

      procedure Decode_Name_Buffer;
      --  Replace "__" by "." in Name_Buffer, and adjust Name_Len accordingly

      ------------------------
      -- Decode_Name_Buffer --
      ------------------------

      procedure Decode_Name_Buffer is
         J : Natural;
         K : Natural;

      begin
         J := 1;
         K := 0;
         while J <= Name_Len loop
            K := K + 1;

            if J < Name_Len
              and then Name_Buffer (J) = '_'
              and then Name_Buffer (J + 1) = '_'
            then
               Name_Buffer (K) := '.';
               J := J + 1;
            else
               Name_Buffer (K) := Name_Buffer (J);
            end if;

            J := J + 1;
         end loop;

         Name_Len := K;
      end Decode_Name_Buffer;

   --  Start of processing for Delete_SCIL_Files

   begin
      --  If parsing was not successful, no Main_Unit is available, so return
      --  immediately.

      if Main_Source_File <= No_Source_File then
         return;
      end if;

      --  Retrieve unit name, and remove old versions of SCIL/<unit>.scil and
      --  SCIL/<unit>__body.scil, ditto for .scilx files.

      Main := Unit (Cunit (Main_Unit));

      case Nkind (Main) is
         when N_Package_Declaration
            | N_Subprogram_Body
            | N_Subprogram_Declaration
         =>
            Unit_Name := Defining_Unit_Name (Specification (Main));

         when N_Package_Body =>
            Unit_Name := Corresponding_Spec (Main);

         when N_Package_Instantiation
            | N_Package_Renaming_Declaration
         =>
            Unit_Name := Defining_Unit_Name (Main);

         --  No SCIL file generated for generic package declarations

         when N_Generic_Package_Declaration
            | N_Generic_Package_Renaming_Declaration
         =>
            return;

         --  Should never happen, but can be ignored in production

         when others =>
            pragma Assert (False);
            return;
      end case;

      case Nkind (Unit_Name) is
         when N_Defining_Identifier =>
            Get_Name_String (Chars (Unit_Name));

         when N_Defining_Program_Unit_Name =>
            Get_Name_String (Chars (Defining_Identifier (Unit_Name)));
            Decode_Name_Buffer;

         --  Should never happen, but can be ignored in production

         when others =>
            pragma Assert (False);
            return;
      end case;

      Delete_File
        ("SCIL/" & Name_Buffer (1 .. Name_Len) & ".scil", Success);
      Delete_File
        ("SCIL/" & Name_Buffer (1 .. Name_Len) & ".scilx", Success);
      Delete_File
        ("SCIL/" & Name_Buffer (1 .. Name_Len) & "__body.scil", Success);
      Delete_File
        ("SCIL/" & Name_Buffer (1 .. Name_Len) & "__body.scilx", Success);
   end Delete_SCIL_Files;

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
