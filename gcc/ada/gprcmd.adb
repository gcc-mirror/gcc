------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               G P R C M D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  A utility used by Makefile.generic to handle multi-language builds.
--  gprcmd provides a set of commands so that the makefiles do not need
--  to depend on unix utilities not available on all targets.

--  The list of commands recognized by gprcmd are:

--    pwd          display current directory
--    to_lower     display next argument in lower case
--    to_absolute  convert pathnames to absolute directories when needed
--    cat          dump contents of a given file
--    extend       handle recursive directories ("/**" notation)
--    deps         post process dependency makefiles
--    stamp        copy file time stamp from file1 to file2
--    prefix       get the prefix of the GNAT installation

with Gnatvsn;
with Osint;   use Osint;
with Namet;   use Namet;

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;               use GNAT.Regpat;


procedure Gprcmd is

   --  ??? comments are thin throughout this unit


   procedure Cat (File : String);
   --  Print the contents of file on standard output.
   --  If the file cannot be read, exit the process with an error code.

   procedure Check_Args (Condition : Boolean);
   --  If Condition is false, print the usage, and exit the process.

   procedure Deps (Objext : String; File : String; GCC : Boolean);
   --  Process $(CC) dependency file. If GCC is True, add a rule so that make
   --  will not complain when a file is removed/added. If GCC is False, add a
   --  rule to recompute the dependency file when needed

   procedure Extend (Dir : String);
   --  If Dir ends with /**, Put all subdirs recursively on standard output,
   --  otherwise put Dir.

   procedure Usage;
   --  Display the command line options and exit the process.

   procedure Copy_Time_Stamp (From, To : String);
   --  Copy file time stamp from file From to file To.

   ---------
   -- Cat --
   ---------

   procedure Cat (File : String) is
      FD     : File_Descriptor;
      Buffer : String_Access;
      Length : Integer;

   begin
      FD := Open_Read (File, Fmode => Binary);

      if FD = Invalid_FD then
         OS_Exit (2);
      end if;

      Length := Integer (File_Length (FD));
      Buffer := new String (1 .. Length);
      Length := Read (FD, Buffer.all'Address, Length);
      Close (FD);
      Put (Buffer.all);
      Free (Buffer);
   end Cat;

   ----------------
   -- Check_Args --
   ----------------

   procedure Check_Args (Condition : Boolean) is
   begin
      if not Condition then
         Usage;
      end if;
   end Check_Args;

   ---------------------
   -- Copy_Time_Stamp --
   ---------------------

   procedure Copy_Time_Stamp (From, To : String) is
      function Copy_Attributes
        (From, To : String;
         Mode     : Integer) return Integer;
      pragma Import (C, Copy_Attributes, "__gnat_copy_attribs");
      --  Mode = 0 - copy only time stamps.
      --  Mode = 1 - copy time stamps and read/write/execute attributes

      FD : File_Descriptor;

   begin
      if not Is_Regular_File (From) then
         return;
      end if;

      FD := Create_File (To, Fmode => Binary);

      if FD = Invalid_FD then
         OS_Exit (2);
      end if;

      Close (FD);

      if Copy_Attributes (From & ASCII.NUL, To & ASCII.NUL, 0) /= 0 then
         OS_Exit (2);
      end if;
   end Copy_Time_Stamp;

   ----------
   -- Deps --
   ----------

   procedure Deps (Objext : String; File : String; GCC : Boolean) is
      Colon      : constant String := ':' & ASCII.LF;
      NL         : constant String := (1 => ASCII.LF);
      Base       : constant String := ' ' & Base_Name (File) & ": ";
      FD         : File_Descriptor;
      Buffer     : String_Access;
      Length     : Integer;
      Obj_Regexp : constant Pattern_Matcher :=
                     Compile ("^.*\" & Objext & ": ");
      Matched    : Match_Array (0 .. 0);
      Start      : Natural;
      First      : Natural;
      Last       : Natural;

   begin
      FD := Open_Read_Write (File, Fmode => Binary);

      if FD = Invalid_FD then
         return;
      end if;

      Length := Integer (File_Length (FD));
      Buffer := new String (1 .. Length);
      Length := Read (FD, Buffer.all'Address, Length);

      if GCC then
         Lseek (FD, 0, Seek_End);
      else
         Close (FD);
         FD := Create_File (File, Fmode => Binary);
      end if;

      Start := Buffer'First;

      while Start <= Buffer'Last loop

         --  Parse Buffer line by line

         while Start < Buffer'Last
           and then (Buffer (Start) = ASCII.CR
                     or else Buffer (Start) = ASCII.LF)
         loop
            Start := Start + 1;
         end loop;

         Last := Start;

         while Last < Buffer'Last
           and then Buffer (Last + 1) /= ASCII.CR
           and then Buffer (Last + 1) /= ASCII.LF
         loop
            Last := Last + 1;
         end loop;

         Match (Obj_Regexp, Buffer (Start .. Last), Matched);

         if GCC then
            if Matched (0) = No_Match then
               First := Start;
            else
               First := Matched (0).Last + 1;
            end if;

            Length := Write (FD, Buffer (First)'Address, Last - First + 1);

            if Start = Last or else Buffer (Last) = '\' then
               Length := Write (FD, NL (1)'Address, NL'Length);
            else
               Length := Write (FD, Colon (1)'Address, Colon'Length);
            end if;

         else
            if Matched (0) = No_Match then
               First := Start;
            else
               Length :=
                 Write (FD, Buffer (Start)'Address,
                        Matched (0).Last - Start - 1);
               Length := Write (FD, Base (Base'First)'Address, Base'Length);
               First := Matched (0).Last + 1;
            end if;

            Length := Write (FD, Buffer (First)'Address, Last - First + 1);
            Length := Write (FD, NL (1)'Address, NL'Length);
         end if;

         Start := Last + 1;
      end loop;

      Close (FD);
      Free (Buffer);
   end Deps;

   ------------
   -- Extend --
   ------------

   procedure Extend (Dir : String) is

      procedure Recursive_Extend (D : String);
      --  Recursively display all subdirectories of D

      ----------------------
      -- Recursive_Extend --
      ----------------------

      procedure Recursive_Extend (D : String) is
         Iter   : Dir_Type;
         Buffer : String (1 .. 8192);
         Last   : Natural;

      begin
         Open (Iter, D);

         loop
            Read (Iter, Buffer, Last);

            exit when Last = 0;

            if Buffer (1 .. Last) /= "."
              and then Buffer (1 .. Last) /= ".."
            then
               declare
                  Abs_Dir : constant String := D & Buffer (1 .. Last);

               begin
                  if Is_Directory (Abs_Dir)
                    and then not Is_Symbolic_Link (Abs_Dir)
                  then
                     Put (' ' & Abs_Dir);
                     Recursive_Extend (Abs_Dir & '/');
                  end if;
               end;
            end if;
         end loop;

         Close (Iter);

      exception
         when Directory_Error =>
            null;
      end Recursive_Extend;

   --  Start of processing for Extend

   begin
      if Dir'Length < 3
        or else (Dir (Dir'Last - 2) /= '/'
                 and then Dir (Dir'Last - 2) /= Directory_Separator)
        or else Dir (Dir'Last - 1 .. Dir'Last) /= "**"
      then
         Put (Dir);
         return;
      end if;

      declare
         D : constant String := Dir (Dir'First .. Dir'Last - 2);
      begin
         Put (D);
         Recursive_Extend (D);
      end;
   end Extend;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line (Standard_Error, "usage: gprcmd cmd [arguments]");
      Put_Line (Standard_Error, "where cmd is one of the following commands:");
      Put_Line (Standard_Error, "  pwd         " &
                                "display current directory");
      Put_Line (Standard_Error, "  to_lower    " &
                                "display next argument in lower case");
      Put_Line (Standard_Error, "  to_absolute " &
                                "convert pathnames to absolute " &
                                "directories when needed");
      Put_Line (Standard_Error, "  cat         " &
                                "dump contents of a given file");
      Put_Line (Standard_Error, "  extend      " &
                                "handle recursive directories " &
                                "(""/**"" notation)");
      Put_Line (Standard_Error, "  deps        " &
                                "post process dependency makefiles");
      Put_Line (Standard_Error, "  stamp       " &
                                "copy file time stamp from file1 to file2");
      OS_Exit (1);
   end Usage;

--  Start of processing for Gprcmd

begin
   Check_Args (Argument_Count > 0);

   declare
      Cmd : constant String := Argument (1);

   begin
      if Cmd = "-v" then

         --  Should this be on Standard_Error ???

         Put (Standard_Error, "GPRCMD ");
         Put (Standard_Error, Gnatvsn.Gnat_Version_String);
         Put_Line (Standard_Error,
                   " Copyright 2002-2004, Free Software Fundation, Inc.");
         Usage;

      elsif Cmd = "pwd" then
         Put (Format_Pathname (Get_Current_Dir, UNIX));

      elsif Cmd = "cat" then
         Check_Args (Argument_Count = 2);
         Cat (Argument (2));

      elsif Cmd = "to_lower" then
         Check_Args (Argument_Count >= 2);

         for J in 2 .. Argument_Count loop
            Put (To_Lower (Argument (J)));

            if J < Argument_Count then
               Put (' ');
            end if;
         end loop;

      elsif Cmd = "to_absolute" then
         Check_Args (Argument_Count > 2);

         declare
            Dir : constant String := Argument (2);

         begin
            for J in 3 .. Argument_Count loop
               if Is_Absolute_Path (Argument (J)) then
                  Put (Format_Pathname (Argument (J), UNIX));
               else
                  Put (Format_Pathname (Normalize_Pathname (Argument (J), Dir),
                                        UNIX));
               end if;

               if J < Argument_Count then
                  Put (' ');
               end if;
            end loop;
         end;

      elsif Cmd = "extend" then
         Check_Args (Argument_Count >= 2);

         declare
            Dir : constant String := Argument (2);

         begin
            for J in 3 .. Argument_Count loop
               if Is_Absolute_Path (Argument (J)) then
                  Extend (Format_Pathname (Argument (J), UNIX));
               else
                  Extend
                    (Format_Pathname (Normalize_Pathname (Argument (J), Dir),
                                      UNIX));
               end if;

               if J < Argument_Count then
                  Put (' ');
               end if;
            end loop;
         end;

      elsif Cmd = "deps" then
         Check_Args (Argument_Count in 3 .. 4);
         Deps (Argument (2), Argument (3), GCC => Argument_Count = 4);

      elsif Cmd = "stamp" then
         Check_Args (Argument_Count = 3);
         Copy_Time_Stamp (Argument (2), Argument (3));

      elsif Cmd = "prefix" then

         --  Find the GNAT prefix. gprcmd is found in <prefix>/bin.
         --  So we find the full path of gprcmd, verify that it is in a
         --  subdirectory "bin", and return the <prefix> if it is the case.
         --  Otherwise, nothing is returned.

         Find_Program_Name;

         declare
            Path  : constant String_Access :=
                      Locate_Exec_On_Path (Name_Buffer (1 .. Name_Len));
            Index : Natural;

         begin
            if Path /= null then
               Index := Path'Last;

               while Index >= Path'First + 4 loop
                  exit when Path (Index) = Directory_Separator;
                  Index := Index - 1;
               end loop;

               if Index > Path'First + 5
                 and then Path (Index - 3 .. Index - 1) = "bin"
                 and then Path (Index - 4) = Directory_Separator
               then
                  --  We have found the <prefix>, return it

                  Put (Path (Path'First .. Index - 5));
               end if;
            end if;
         end;
      end if;
   end;
end Gprcmd;
