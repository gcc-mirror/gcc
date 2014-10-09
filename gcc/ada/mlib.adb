------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M L I B                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2014, AdaCore                     --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces.C.Strings;
with System;

with Opt;
with Output; use Output;

with MLib.Utl; use MLib.Utl;

with Prj.Com;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body MLib is

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library
     (Ofiles      : Argument_List;
      Output_File : String;
      Output_Dir  : String)
   is
   begin
      if Opt.Verbose_Mode and not Opt.Quiet_Output then
         Write_Line ("building a library...");
         Write_Str  ("   make ");
         Write_Line (Output_File);
      end if;

      Ar (Output_Dir &
          "lib" & Output_File & ".a", Objects => Ofiles);
   end Build_Library;

   ------------------------
   -- Check_Library_Name --
   ------------------------

   procedure Check_Library_Name (Name : String) is
   begin
      if Name'Length = 0 then
         Prj.Com.Fail ("library name cannot be empty");
      end if;

      if Name'Length > Max_Characters_In_Library_Name then
         Prj.Com.Fail ("illegal library name """
                       & Name
                       & """: too long");
      end if;

      if not Is_Letter (Name (Name'First)) then
         Prj.Com.Fail ("illegal library name """
                       & Name
                       & """: should start with a letter");
      end if;

      for Index in Name'Range loop
         if not Is_Alphanumeric (Name (Index)) then
            Prj.Com.Fail ("illegal library name """
                          & Name
                          & """: should include only letters and digits");
         end if;
      end loop;
   end Check_Library_Name;

   --------------------
   -- Copy_ALI_Files --
   --------------------

   procedure Copy_ALI_Files
     (Files      : Argument_List;
      To         : Path_Name_Type;
      Interfaces : String_List)
   is
      Success      : Boolean := False;
      To_Dir       : constant String := Get_Name_String (To);
      Is_Interface : Boolean := False;

      procedure Verbose_Copy (Index : Positive);
      --  In verbose mode, output a message that the indexed file is copied
      --  to the destination directory.

      ------------------
      -- Verbose_Copy --
      ------------------

      procedure Verbose_Copy (Index : Positive) is
      begin
         if Opt.Verbose_Mode then
            Write_Str ("Copying """);
            Write_Str (Files (Index).all);
            Write_Str (""" to """);
            Write_Str (To_Dir);
            Write_Line ("""");
         end if;
      end Verbose_Copy;

   --  Start of processing for Copy_ALI_Files

   begin
      if Interfaces'Length = 0 then

         --  If there are no Interfaces, copy all the ALI files as is

         for Index in Files'Range loop
            Verbose_Copy (Index);
            Set_Writable
              (To_Dir &
               Directory_Separator &
               Base_Name (Files (Index).all));
            Copy_File
              (Files (Index).all,
               To_Dir,
               Success,
               Mode => Overwrite,
               Preserve => Preserve);

            exit when not Success;
         end loop;

      else
         --  Copy only the interface ALI file, and put the special indicator
         --  "SL" on the P line.

         for Index in Files'Range loop

            declare
               File_Name : String := Base_Name (Files (Index).all);

            begin
               Canonical_Case_File_Name (File_Name);

               --  Check if this is one of the interface ALIs

               Is_Interface := False;

               for Index in Interfaces'Range loop
                  if File_Name = Interfaces (Index).all then
                     Is_Interface := True;
                     exit;
                  end if;
               end loop;

               --  If it is an interface ALI, copy line by line. Insert
               --  the interface indication at the end of the P line.
               --  Do not copy ALI files that are not Interfaces.

               if Is_Interface then
                  Success := False;
                  Verbose_Copy (Index);
                  Set_Writable
                    (To_Dir &
                     Directory_Separator &
                     Base_Name (Files (Index).all));

                  declare
                     FD           : File_Descriptor;
                     Len          : Integer;
                     Actual_Len   : Integer;
                     S            : String_Access;
                     Curr         : Natural;
                     P_Line_Found : Boolean;
                     Status       : Boolean;

                  begin
                     --  Open the file

                     Name_Len := Files (Index)'Length;
                     Name_Buffer (1 .. Name_Len) := Files (Index).all;
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := ASCII.NUL;

                     FD := Open_Read (Name_Buffer'Address, Binary);

                     if FD /= Invalid_FD then
                        Len := Integer (File_Length (FD));

                        --  ??? Why "+3" here

                        S := new String (1 .. Len + 3);

                        --  Read the file. This loop is probably not necessary
                        --  since on most (all?) targets, the whole file is
                        --  read in at once, but we have encountered systems
                        --  in the past where this was not true, and we retain
                        --  this loop in case we encounter that in the future.

                        Curr := S'First;
                        while Curr <= Len loop
                           Actual_Len := Read (FD, S (Curr)'Address, Len);

                           --  Exit if we could not read for some reason

                           exit when Actual_Len = 0;

                           Curr := Curr + Actual_Len;
                        end loop;

                        --  We are done with the input file, so we close it
                        --  ignoring any bad status.

                        Close (FD, Status);

                        P_Line_Found := False;

                        --  Look for the P line. When found, add marker SL
                        --  at the beginning of the P line.

                        for Index in 1 .. Len - 3 loop
                           if (S (Index) = ASCII.LF
                                 or else
                               S (Index) = ASCII.CR)
                             and then S (Index + 1) = 'P'
                           then
                              S (Index + 5 .. Len + 3) := S (Index + 2 .. Len);
                              S (Index + 2 .. Index + 4) := " SL";
                              P_Line_Found := True;
                              exit;
                           end if;
                        end loop;

                        if P_Line_Found then

                           --  Create new modified ALI file

                           Name_Len := To_Dir'Length;
                           Name_Buffer (1 .. Name_Len) := To_Dir;
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) := Directory_Separator;
                           Name_Buffer
                             (Name_Len + 1 .. Name_Len + File_Name'Length) :=
                                File_Name;
                           Name_Len := Name_Len + File_Name'Length + 1;
                           Name_Buffer (Name_Len) := ASCII.NUL;

                           FD := Create_File (Name_Buffer'Address, Binary);

                           --  Write the modified text and close the newly
                           --  created file.

                           if FD /= Invalid_FD then
                              Actual_Len := Write (FD, S (1)'Address, Len + 3);

                              Close (FD, Status);

                              --  Set Success to True only if the newly
                              --  created file has been correctly written.

                              Success := Status and then Actual_Len = Len + 3;

                              if Success then

                                 --  Set_Read_Only is used here, rather than
                                 --  Set_Non_Writable, so that gprbuild can
                                 --  he compiled with older compilers.

                                 Set_Read_Only
                                   (Name_Buffer (1 .. Name_Len - 1));
                              end if;
                           end if;
                        end if;
                     end if;
                  end;

               --  This is not an interface ALI

               else
                  Success := True;
               end if;
            end;

            if not Success then
               Prj.Com.Fail ("could not copy ALI files to library dir");
            end if;
         end loop;
      end if;
   end Copy_ALI_Files;

   ----------------------
   -- Create_Sym_Links --
   ----------------------

   procedure Create_Sym_Links
     (Lib_Path    : String;
      Lib_Version : String;
      Lib_Dir     : String;
      Maj_Version : String)
   is
      function Symlink
        (Oldpath : System.Address;
         Newpath : System.Address) return Integer;
      pragma Import (C, Symlink, "__gnat_symlink");

      Version_Path : String_Access;

      Success : Boolean;
      Result  : Integer;
      pragma Unreferenced (Success, Result);

   begin
      Version_Path := new String (1 .. Lib_Version'Length + 1);
      Version_Path (1 .. Lib_Version'Length) := Lib_Version;
      Version_Path (Version_Path'Last)       := ASCII.NUL;

      if Maj_Version'Length = 0 then
         declare
            Newpath : String (1 .. Lib_Path'Length + 1);
         begin
            Newpath (1 .. Lib_Path'Length) := Lib_Path;
            Newpath (Newpath'Last)         := ASCII.NUL;
            Delete_File (Lib_Path, Success);
            Result := Symlink (Version_Path (1)'Address, Newpath'Address);
         end;

      else
         declare
            Newpath1 : String (1 .. Lib_Path'Length + 1);
            Maj_Path : constant String :=
                         Lib_Dir & Directory_Separator & Maj_Version;
            Newpath2 : String (1 .. Maj_Path'Length + 1);
            Maj_Ver  : String (1 .. Maj_Version'Length + 1);

         begin
            Newpath1 (1 .. Lib_Path'Length) := Lib_Path;
            Newpath1 (Newpath1'Last)        := ASCII.NUL;

            Newpath2 (1 .. Maj_Path'Length) := Maj_Path;
            Newpath2 (Newpath2'Last)        := ASCII.NUL;

            Maj_Ver (1 .. Maj_Version'Length) := Maj_Version;
            Maj_Ver (Maj_Ver'Last)            := ASCII.NUL;

            Delete_File (Maj_Path, Success);

            Result := Symlink (Version_Path (1)'Address, Newpath2'Address);

            Delete_File (Lib_Path, Success);

            Result := Symlink (Maj_Ver'Address, Newpath1'Address);
         end;
      end if;
   end Create_Sym_Links;

   --------------------------------
   -- Linker_Library_Path_Option --
   --------------------------------

   function Linker_Library_Path_Option return String_Access is

      Run_Path_Option_Ptr : Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Run_Path_Option_Ptr, "__gnat_run_path_option");
      --  Pointer to string representing the native linker option which
      --  specifies the path where the dynamic loader should find shared
      --  libraries. Equal to null string if this system doesn't support it.

      S : constant String := Interfaces.C.Strings.Value (Run_Path_Option_Ptr);

   begin
      if S'Length = 0 then
         return null;
      else
         return new String'(S);
      end if;
   end Linker_Library_Path_Option;

   -------------------
   -- Major_Id_Name --
   -------------------

   function Major_Id_Name
     (Lib_Filename : String;
      Lib_Version  : String)
      return String
   is
      Maj_Version : constant String := Lib_Version;
      Last_Maj    : Positive;
      Last        : Positive;
      Ok_Maj      : Boolean := False;

   begin
      Last_Maj := Maj_Version'Last;
      while Last_Maj > Maj_Version'First loop
         if Maj_Version (Last_Maj) in '0' .. '9' then
            Last_Maj := Last_Maj - 1;

         else
            Ok_Maj := Last_Maj /= Maj_Version'Last and then
            Maj_Version (Last_Maj) = '.';

            if Ok_Maj then
               Last_Maj := Last_Maj - 1;
            end if;

            exit;
         end if;
      end loop;

      if Ok_Maj then
         Last := Last_Maj;
         while Last > Maj_Version'First loop
            if Maj_Version (Last) in '0' .. '9' then
               Last := Last - 1;

            else
               Ok_Maj := Last /= Last_Maj and then
               Maj_Version (Last) = '.';

               if Ok_Maj then
                  Last := Last - 1;
                  Ok_Maj :=
                    Maj_Version (Maj_Version'First .. Last) = Lib_Filename;
               end if;

               exit;
            end if;
         end loop;
      end if;

      if Ok_Maj then
         return Maj_Version (Maj_Version'First .. Last_Maj);
      else
         return "";
      end if;
   end Major_Id_Name;

   -------------------------------
   -- Separate_Run_Path_Options --
   -------------------------------

   function Separate_Run_Path_Options return Boolean is
      Separate_Paths : Boolean;
      for Separate_Paths'Size use Character'Size;
      pragma Import (C, Separate_Paths, "__gnat_separate_run_path_options");
   begin
      return Separate_Paths;
   end Separate_Run_Path_Options;

end MLib;
