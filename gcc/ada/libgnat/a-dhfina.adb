------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.DIRECTORIES.HIERARCHICAL_FILE_NAMES                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- In particular,  you can freely  distribute your programs  built with the --
-- GNAT Pro compiler, including any required library run-time units,  using --
-- any licensing terms  of your choosing.  See the AdaCore Software License --
-- for full details.                                                        --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Directories.Validity; use Ada.Directories.Validity;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with System;                   use System;

package body Ada.Directories.Hierarchical_File_Names is

   Dir_Separator : constant Character;
   pragma Import (C, Dir_Separator, "__gnat_dir_separator");
   --  Running system default directory separator

   -----------------
   -- Subprograms --
   -----------------

   function Equivalent_File_Names
     (Left  : String;
      Right : String)
      return Boolean;
   --  Perform an OS-independent comparison between two file paths

   function Is_Absolute_Path (Name : String) return Boolean;
   --  Returns True if Name is an absolute path name, i.e. it designates a
   --  file or directory absolutely rather than relative to another directory.

   ---------------------------
   -- Equivalent_File_Names --
   ---------------------------

   function Equivalent_File_Names
     (Left  : String;
      Right : String)
      return Boolean
   is
   begin
      --  Check the validity of the input paths

      if not Is_Valid_Path_Name (Left)
        or else not Is_Valid_Path_Name (Right)
      then
         return False;
      end if;

      --  Normalize the paths by removing any trailing directory separators and
      --  perform the comparison.

      declare
         Normal_Left  : constant String :=
           (if Index (Left, Dir_Separator & "", Strings.Backward) = Left'Last
              and then not Is_Root_Directory_Name (Left)
            then
               Left (Left'First .. Left'Last - 1)
            else
               Left);

         Normal_Right : constant String :=
           (if Index (Right, Dir_Separator & "", Strings.Backward) = Right'Last
              and then not Is_Root_Directory_Name (Right)
            then
               Right (Right'First .. Right'Last - 1)
            else
               Right);
      begin
         --  Within Windows we assume case insensitivity

         if not Windows then
            return Normal_Left = Normal_Right;
         end if;

         --  Otherwise do a straight comparison

         return To_Lower (Normal_Left) = To_Lower (Normal_Right);
      end;
   end Equivalent_File_Names;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (Name : String) return Boolean is
      function Is_Absolute_Path
        (Name   : Address;
         Length : Integer) return Integer;
      pragma Import (C, Is_Absolute_Path, "__gnat_is_absolute_path");
   begin
      return Is_Absolute_Path (Name'Address, Name'Length) /= 0;
   end Is_Absolute_Path;

   --------------------
   -- Is_Simple_Name --
   --------------------

   function Is_Simple_Name (Name : String) return Boolean is
   begin
      --  Verify the file path name is valid and that it is not a root

      if not Is_Valid_Path_Name (Name)
        or else Is_Root_Directory_Name (Name)
      then
         return False;
      end if;

      --  Check for the special paths "." and "..", which are considered simple

      if Is_Parent_Directory_Name (Name)
        or else Is_Current_Directory_Name (Name)
      then
         return True;
      end if;

      --  Perform a comparison with the calculated simple path name

      return Equivalent_File_Names (Simple_Name (Name), Name);
   end Is_Simple_Name;

   ----------------------------
   -- Is_Root_Directory_Name --
   ----------------------------

   function Is_Root_Directory_Name (Name : String) return Boolean is
   begin
      --  Check if the path name is a root directory by looking for a slash in
      --  the general case, and a drive letter in the case of Windows.

      return Name = "/"
               or else
                 (Windows
                   and then
                     (Name = "\"
                       or else
                         (Name'Length = 3
                           and then Name (Name'Last - 1) = ':'
                           and then Name (Name'Last) in '/' | '\'
                           and then (Name (Name'First) in 'a' .. 'z'
                                      or else
                                        Name (Name'First) in 'A' .. 'Z'))
                       or else
                         (Name'Length = 2
                           and then Name (Name'Last) = ':'
                           and then (Name (Name'First) in 'a' .. 'z'
                                      or else
                                        Name (Name'First) in 'A' .. 'Z'))));
   end Is_Root_Directory_Name;

   ------------------------------
   -- Is_Parent_Directory_Name --
   ------------------------------

   function Is_Parent_Directory_Name (Name : String) return Boolean is
   begin
      return Name = "..";
   end Is_Parent_Directory_Name;

   -------------------------------
   -- Is_Current_Directory_Name --
   -------------------------------

   function Is_Current_Directory_Name (Name : String) return Boolean is
   begin
      return Name = ".";
   end Is_Current_Directory_Name;

   ------------------
   -- Is_Full_Name --
   ------------------

   function Is_Full_Name (Name : String) return Boolean is
   begin
      return Equivalent_File_Names (Full_Name (Name), Name);
   end Is_Full_Name;

   ----------------------
   -- Is_Relative_Name --
   ----------------------

   function Is_Relative_Name (Name : String) return Boolean is
   begin
      return not Is_Absolute_Path (Name)
               and then Is_Valid_Path_Name (Name);
   end Is_Relative_Name;

   -----------------------
   -- Initial_Directory --
   -----------------------

   function Initial_Directory (Name : String) return String is
      Start : constant Integer := Index (Name, Dir_Separator & "");
   begin
      --  Verify path name

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';
      end if;

      --  When there is no starting directory separator or the path name is a
      --  root directory then the path name is already simple - so return it.

      if Is_Root_Directory_Name (Name) or else Start = 0 then
         return Name;
      end if;

      --  When the initial directory of the path name is a root directory then
      --  the starting directory separator is part of the result so we must
      --  return it in the slice.

      if Is_Root_Directory_Name (Name (Name'First .. Start)) then
         return Name (Name'First .. Start);
      end if;

      --  Otherwise we grab a slice up to the starting directory separator

      return Name (Name'First .. Start - 1);
   end Initial_Directory;

   -------------------
   -- Relative_Name --
   -------------------

   function Relative_Name (Name : String) return String is
   begin
      --  We cannot derive a relative name if Name does not exist

      if not Is_Relative_Name (Name)
        and then not Is_Valid_Path_Name (Name)
      then
         raise Name_Error with "invalid relative path name """ & Name & '"';
      end if;

      --  Name only has a single part and thus cannot be made relative

      if Is_Simple_Name (Name)
        or else Is_Root_Directory_Name (Name)
      then
         raise Name_Error with
           "relative path name """ & Name & """ is composed of a single part";
      end if;

      --  Trim the input according to the initial directory and maintain proper
      --  directory separation due to the fact that root directories may
      --  contain separators.

      declare
         Init_Dir : constant String := Initial_Directory (Name);
      begin
         if Init_Dir (Init_Dir'Last) = Dir_Separator then
            return Name (Name'First + Init_Dir'Length .. Name'Last);
         end if;

         return Name (Name'First + Init_Dir'Length + 1 .. Name'Last);
      end;
   end Relative_Name;

   -------------
   -- Compose --
   -------------

   function Compose
     (Directory     : String := "";
      Relative_Name : String;
      Extension     : String := "") return String
   is
      --  Append a directory separator if none is present

      Separated_Dir : constant String :=
        (if    Directory = "" then ""
         elsif Directory (Directory'Last) = Dir_Separator then Directory
         else  Directory & Dir_Separator);
   begin
      --  Check that relative name is valid

      if not Is_Relative_Name (Relative_Name) then
         raise Name_Error with
           "invalid relative path name """ & Relative_Name & '"';
      end if;

      --  Check that directory is valid

      if Separated_Dir /= ""
        and then not Is_Valid_Path_Name (Separated_Dir & Relative_Name)
      then
         raise Name_Error with
           "invalid path composition """ & Separated_Dir & Relative_Name & '"';
      end if;

      --  Check that the extension is valid

      if Extension /= ""
        and then not Is_Valid_Path_Name
                       (Separated_Dir & Relative_Name & Extension)
      then
         raise Name_Error with
           "invalid path composition """
             & Separated_Dir & Relative_Name & Extension & '"';
      end if;

      --  Concatenate the result

      return Separated_Dir & Relative_Name & Extension;
   end Compose;

end Ada.Directories.Hierarchical_File_Names;
