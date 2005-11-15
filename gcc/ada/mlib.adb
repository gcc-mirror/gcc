------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M L I B                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2005, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces.C.Strings;

with Hostparm;
with Opt;
with Output; use Output;
with Namet;  use Namet;

with MLib.Utl; use MLib.Utl;

with Prj.Com;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body MLib is

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library
     (Ofiles      : Argument_List;
      Afiles      : Argument_List;
      Output_File : String;
      Output_Dir  : String)
   is
      pragma Warnings (Off, Afiles);

   begin
      if not Opt.Quiet_Output then
         Write_Line ("building a library...");
         Write_Str  ("   make ");
         Write_Line (Output_File);
      end if;

      Ar (Output_Dir & "/lib" & Output_File & ".a", Objects => Ofiles);
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
         Prj.Com.Fail ("illegal library name """, Name, """: too long");
      end if;

      if not Is_Letter (Name (Name'First)) then
         Prj.Com.Fail ("illegal library name """,
                       Name,
                       """: should start with a letter");
      end if;

      for Index in Name'Range loop
         if not Is_Alphanumeric (Name (Index)) then
            Prj.Com.Fail ("illegal library name """,
                          Name,
                          """: should include only letters and digits");
         end if;
      end loop;
   end Check_Library_Name;

   --------------------
   -- Copy_ALI_Files --
   --------------------

   procedure Copy_ALI_Files
     (Files      : Argument_List;
      To         : Name_Id;
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

   begin
      if Interfaces'Length = 0 then

         --  If there are no Interfaces, copy all the ALI files as is

         for Index in Files'Range loop
            Verbose_Copy (Index);
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

                  declare
                     FD         : File_Descriptor;
                     Len        : Integer;
                     Actual_Len : Integer;
                     S          : String_Access;
                     Curr       : Natural;
                     P_Line_Found : Boolean;
                     Status     : Boolean;

                  begin
                     --  Open the file

                     Name_Len := Files (Index)'Length;
                     Name_Buffer (1 .. Name_Len) := Files (Index).all;
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := ASCII.NUL;

                     FD := Open_Read (Name_Buffer'Address, Binary);

                     if FD /= Invalid_FD then
                        Len := Integer (File_Length (FD));

                        S := new String (1 .. Len + 3);

                        --  Read the file. Note that the loop is not necessary
                        --  since the whole file is read at once except on VMS.

                        Curr := 1;
                        Actual_Len := Len;

                        while Actual_Len /= 0 loop
                           Actual_Len := Read (FD, S (Curr)'Address, Len);
                           Curr := Curr + Actual_Len;
                        end loop;

                        --  We are done with the input file, so we close it

                        Close (FD, Status);
                        --  We simply ignore any bad status

                        P_Line_Found := False;

                        --  Look for the P line. When found, add marker SL
                        --  at the beginning of the P line.

                        for Index in 1 .. Len - 3 loop
                           if (S (Index) = ASCII.LF or else
                                 S (Index) = ASCII.CR)
                             and then
                               S (Index + 1) = 'P'
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

                              Success := Status and Actual_Len = Len + 3;

                              if Success then
                                 Set_Read_Only (
                                   Name_Buffer (1 .. Name_Len - 1));
                              end if;
                           end if;
                        end if;
                     end if;
                  end;

               else
                  --  This is not an interface ALI

                  Success := True;

               end if;
            end;

            if not Success then
               Prj.Com.Fail ("could not copy ALI files to library dir");
            end if;
         end loop;
      end if;
   end Copy_ALI_Files;

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

--  Package elaboration

begin
   --  Copy_Attributes always fails on VMS

   if Hostparm.OpenVMS then
      Preserve := None;
   end if;
end MLib;
