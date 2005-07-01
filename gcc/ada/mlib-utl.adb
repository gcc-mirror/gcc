------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             M L I B . U T L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2002-2004, Ada Core Technologies, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with MLib.Fil; use MLib.Fil;
with MLib.Tgt; use MLib.Tgt;

with Namet;    use Namet;
with Opt;
with Osint;
with Output;   use Output;

with GNAT;     use GNAT;

package body MLib.Utl is

   Initialized : Boolean := False;

   Gcc_Name : constant String := "gcc";
   Gcc_Exec : OS_Lib.String_Access;

   Ar_Name    : OS_Lib.String_Access;
   Ar_Exec    : OS_Lib.String_Access;
   Ar_Options : OS_Lib.String_List_Access;

   Ranlib_Name    : OS_Lib.String_Access;
   Ranlib_Exec    : OS_Lib.String_Access := null;
   Ranlib_Options : OS_Lib.String_List_Access := null;

   procedure Initialize;
   --  Look for the tools in the path and record the full path for each one

   --------
   -- Ar --
   --------

   procedure Ar (Output_File : String; Objects : Argument_List) is
      Full_Output_File : constant String :=
                             Ext_To (Output_File, Archive_Ext);

      Arguments : OS_Lib.Argument_List_Access;

      Success   : Boolean;

      Line_Length : Natural := 0;

   begin
      Utl.Initialize;

      Arguments :=
        new String_List (1 .. 1 + Ar_Options'Length + Objects'Length);
      Arguments (1 .. Ar_Options'Length) := Ar_Options.all; --  "ar cr ..."
      Arguments (Ar_Options'Length + 1) := new String'(Full_Output_File);
      Arguments (Ar_Options'Length + 2 .. Arguments'Last) := Objects;

      Delete_File (Full_Output_File);

      if not Opt.Quiet_Output then
         Write_Str (Ar_Name.all);
         Line_Length := Ar_Name'Length;

         for J in Arguments'Range loop

            --  Make sure the Output buffer does not overflow

            if Line_Length + 1 + Arguments (J)'Length >
                 Integer (Opt.Max_Line_Length)
            then
               Write_Eol;
               Line_Length := 0;
            end if;

            Write_Char (' ');
            Write_Str  (Arguments (J).all);
            Line_Length := Line_Length + 1 + Arguments (J)'Length;
         end loop;

         Write_Eol;
      end if;

      OS_Lib.Spawn (Ar_Exec.all, Arguments.all, Success);

      if not Success then
         Fail (Ar_Name.all, " execution error.");
      end if;

      --  If we have found ranlib, run it over the library

      if Ranlib_Exec /= null then
         if not Opt.Quiet_Output then
            Write_Str  (Ranlib_Name.all);
            Write_Char (' ');
            Write_Line (Arguments (Ar_Options'Length + 1).all);
         end if;

         OS_Lib.Spawn
           (Ranlib_Exec.all,
            Ranlib_Options.all & (Arguments (Ar_Options'Length + 1)),
            Success);

         if not Success then
            Fail (Ranlib_Name.all, " execution error.");
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
      Options     : Argument_List;
      Options_2   : Argument_List;
      Driver_Name : Name_Id := No_Name)
   is
      Arguments :
        OS_Lib.Argument_List
          (1 .. 7 + Objects'Length + Options'Length + Options_2'Length);

      A       : Natural := 0;
      Success : Boolean;

      Out_Opt : constant OS_Lib.String_Access :=
                  new String'("-o");
      Out_V   : constant OS_Lib.String_Access :=
                  new String'(Output_File);
      Lib_Dir : constant OS_Lib.String_Access :=
                  new String'("-L" & Lib_Directory);
      Lib_Opt : constant OS_Lib.String_Access :=
                  new String'(Dynamic_Option);

      Driver  : String_Access;
   begin
      Utl.Initialize;

      if Driver_Name = No_Name then
         Driver := Gcc_Exec;

      else
         Driver := OS_Lib.Locate_Exec_On_Path (Get_Name_String (Driver_Name));

         if Driver = null then
            Fail (Get_Name_String (Driver_Name), " not found in path");
         end if;
      end if;

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

      A := A + Options_2'Length;
      Arguments (A - Options_2'Length + 1 .. A) := Options_2;

      if not Opt.Quiet_Output then
         Write_Str (Driver.all);

         for J in 1 .. A loop
            Write_Char (' ');
            Write_Str  (Arguments (J).all);
         end loop;

         Write_Eol;
      end if;

      OS_Lib.Spawn (Driver.all, Arguments (1 .. A), Success);

      if not Success then
         if Driver_Name = No_Name then
            Fail (Gcc_Name, " execution error");

         else
            Fail (Get_Name_String (Driver_Name), " execution error");
         end if;
      end if;
   end Gcc;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
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

         Ar_Name := new String'(Archive_Builder);
         Ar_Exec := OS_Lib.Locate_Exec_On_Path (Ar_Name.all);

         if Ar_Exec = null then
            Fail (Ar_Name.all, " not found in path");

         elsif Opt.Verbose_Mode then
            Write_Str  ("found ");
            Write_Line (Ar_Exec.all);
         end if;

         Ar_Options := Archive_Builder_Options;

         --  ranlib

         Ranlib_Name := new String'(Archive_Indexer);

         if Ranlib_Name'Length > 0 then
            Ranlib_Exec := OS_Lib.Locate_Exec_On_Path (Ranlib_Name.all);

            if Ranlib_Exec /= null and then Opt.Verbose_Mode then
               Write_Str ("found ");
               Write_Line (Ranlib_Exec.all);
            end if;
         end if;

         Ranlib_Options := Archive_Indexer_Options;
      end if;
   end Initialize;

   -------------------
   -- Lib_Directory --
   -------------------

   function Lib_Directory return String is
      Libgnat : constant String := Tgt.Libgnat;

   begin
      Name_Len := Libgnat'Length;
      Name_Buffer (1 .. Name_Len) := Libgnat;
      Get_Name_String (Osint.Find_File (Name_Enter, Osint.Library));

      --  Remove libgnat.a

      return Name_Buffer (1 .. Name_Len - Libgnat'Length);
   end Lib_Directory;

end MLib.Utl;
