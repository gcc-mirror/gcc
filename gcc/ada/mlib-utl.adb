------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . U T L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2002, Ada Core Technologies, Inc.             --
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
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with MLib.Fil;
with MLib.Tgt;
with Namet;  use Namet;
with Opt;
with Osint;  use Osint;
with Output; use Output;

package body MLib.Utl is

   use GNAT;

   package Files  renames MLib.Fil;
   package Target renames MLib.Tgt;

   Initialized   : Boolean := False;

   Gcc_Name      : constant String := "gcc";
   Gcc_Exec      : OS_Lib.String_Access;

   Ar_Name       : constant String := "ar";
   Ar_Exec       : OS_Lib.String_Access;

   Ranlib_Name   : constant String := "ranlib";
   Ranlib_Exec   : OS_Lib.String_Access;

   procedure Initialize;
   --  Look for the tools in the path and record the full path for each one

   --------
   -- Ar --
   --------

   procedure Ar (Output_File : String; Objects : Argument_List) is
      Create_Add_Opt : OS_Lib.String_Access := new String' ("cr");

      Full_Output_File : constant String :=
                             Files.Ext_To (Output_File, Target.Archive_Ext);

      Arguments : OS_Lib.Argument_List (1 .. 2 + Objects'Length);
      Success   : Boolean;

   begin
      Initialize;

      Arguments (1) := Create_Add_Opt; --  "ar cr ..."
      Arguments (2) := new String'(Full_Output_File);
      Arguments (3 .. Arguments'Last) := Objects;

      Delete_File (Full_Output_File);

      if not Opt.Quiet_Output then
         Write_Str (Ar_Name);

         for J in Arguments'Range loop
            Write_Char (' ');
            Write_Str  (Arguments (J).all);
         end loop;

         Write_Eol;
      end if;

      OS_Lib.Spawn (Ar_Exec.all, Arguments, Success);

      if not Success then
         Fail (Ar_Name, " execution error.");
      end if;

      --  If we have found ranlib, run it over the library

      if Ranlib_Exec /= null then
         if not Opt.Quiet_Output then
            Write_Str  (Ranlib_Name);
            Write_Char (' ');
            Write_Line (Arguments (2).all);
         end if;

         OS_Lib.Spawn (Ranlib_Exec.all, (1 => Arguments (2)), Success);

         if not Success then
            Fail (Ranlib_Name, " execution error.");
         end if;
      end if;
   end Ar;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Filename : in String) is
      File   : constant String := Filename & ASCII.Nul;
      Success : Boolean;

   begin
      OS_Lib.Delete_File (File'Address, Success);

      if Opt.Verbose_Mode then
         if Success then
            Write_Str ("deleted ");

         else
            Write_Str ("could not delete ");
         end if;

         Write_Line (Filename);
      end if;
   end Delete_File;

   ---------
   -- Gcc --
   ---------

   procedure Gcc
     (Output_File : String;
      Objects     : Argument_List;
      Options     : Argument_List)
   is
      Arguments : OS_Lib.Argument_List
                    (1 .. 7 + Objects'Length + Options'Length);

      A         : Natural := 0;
      Success   : Boolean;
      Out_Opt   : OS_Lib.String_Access := new String' ("-o");
      Out_V     : OS_Lib.String_Access := new String' (Output_File);
      Lib_Dir   : OS_Lib.String_Access := new String' ("-L" & Lib_Directory);
      Lib_Opt   : OS_Lib.String_Access := new String' (Target.Dynamic_Option);

   begin
      Initialize;

      if Lib_Opt'Length /= 0 then
         A := A + 1;
         Arguments (A) := Lib_Opt;
      end if;

      A := A + 1;
      Arguments (A) := Out_Opt;

      A := A + 1;
      Arguments (A) := Out_V;

      A := A + 1;
      Arguments (A) := Lib_Dir;

      A := A + Options'Length;
      Arguments (A - Options'Length + 1 .. A) := Options;

      A := A + Objects'Length;
      Arguments (A - Objects'Length + 1 .. A) := Objects;

      if not Opt.Quiet_Output then
         Write_Str (Gcc_Exec.all);

         for J in 1 .. A loop
            Write_Char (' ');
            Write_Str  (Arguments (J).all);
         end loop;

         Write_Eol;
      end if;

      OS_Lib.Spawn (Gcc_Exec.all, Arguments (1 .. A), Success);

      if not Success then
         Fail (Gcc_Name, " execution error");
      end if;
   end Gcc;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      use type OS_Lib.String_Access;

   begin
      if not Initialized then
         Initialized := True;

         --  gcc

         Gcc_Exec := OS_Lib.Locate_Exec_On_Path (Gcc_Name);

         if Gcc_Exec = null then

            Fail (Gcc_Name, " not found in path");

         elsif Opt.Verbose_Mode then
            Write_Str  ("found ");
            Write_Line (Gcc_Exec.all);
         end if;

         --  ar

         Ar_Exec := OS_Lib.Locate_Exec_On_Path (Ar_Name);

         if Ar_Exec = null then

            Fail (Ar_Name, " not found in path");

         elsif Opt.Verbose_Mode then
            Write_Str  ("found ");
            Write_Line (Ar_Exec.all);
         end if;

         --  ranlib

         Ranlib_Exec := OS_Lib.Locate_Exec_On_Path (Ranlib_Name);

         if Ranlib_Exec /= null and then Opt.Verbose_Mode then
            Write_Str ("found ");
            Write_Line (Ranlib_Exec.all);
         end if;

      end if;

   end Initialize;

   -------------------
   -- Lib_Directory --
   -------------------

   function Lib_Directory return String is
      Libgnat : constant String := Target.Libgnat;

   begin
      Name_Len := Libgnat'Length;
      Name_Buffer (1 .. Name_Len) := Libgnat;
      Get_Name_String (Find_File (Name_Enter, Library));

      --  Remove libgnat.a

      return Name_Buffer (1 .. Name_Len - Libgnat'Length);
   end Lib_Directory;

end MLib.Utl;
