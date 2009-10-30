------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      A D A . D I R E C T O R I E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2009, Free Software Foundation, Inc.         --
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
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;               use Ada.Calendar;
with Ada.Calendar.Formatting;    use Ada.Calendar.Formatting;
with Ada.Directories.Validity;   use Ada.Directories.Validity;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

with System.CRTL;                use System.CRTL;
with System.OS_Lib;              use System.OS_Lib;
with System.Regexp;              use System.Regexp;

with System;

package body Ada.Directories is

   Filename_Max : constant Integer := 1024;
   --  1024 is the value of FILENAME_MAX in stdio.h

   type Dir_Type_Value is new System.Address;
   --  This is the low-level address directory structure as returned by the C
   --  opendir routine.

   No_Dir : constant Dir_Type_Value := Dir_Type_Value (System.Null_Address);

   Dir_Separator : constant Character;
   pragma Import (C, Dir_Separator, "__gnat_dir_separator");
   --  Running system default directory separator

   Dir_Seps : constant Ada.Strings.Maps.Character_Set :=
                Ada.Strings.Maps.To_Set ("/\");
   --  UNIX and DOS style directory separators

   Max_Path : Integer;
   pragma Import (C, Max_Path, "__gnat_max_path_len");
   --  The maximum length of a path

   type Search_Data is record
      Is_Valid      : Boolean := False;
      Name          : Unbounded_String;
      Pattern       : Regexp;
      Filter        : Filter_Type;
      Dir           : Dir_Type_Value := No_Dir;
      Entry_Fetched : Boolean := False;
      Dir_Entry     : Directory_Entry_Type;
   end record;
   --  The current state of a search

   Empty_String : constant String := (1 .. 0 => ASCII.NUL);
   --  Empty string, returned by function Extension when there is no extension

   procedure Free is new Ada.Unchecked_Deallocation (Search_Data, Search_Ptr);

   procedure Close (Dir : Dir_Type_Value);

   function File_Exists (Name : String) return Boolean;
   --  Returns True if the named file exists

   procedure Fetch_Next_Entry (Search : Search_Type);
   --  Get the next entry in a directory, setting Entry_Fetched if successful
   --  or resetting Is_Valid if not.

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Name : String) return String is
      Simple : constant String := Simple_Name (Name);
      --  Simple'First is guaranteed to be 1

   begin
      --  Look for the last dot in the file name and return the part of the
      --  file name preceding this last dot. If the first dot is the first
      --  character of the file name, the base name is the empty string.

      for Pos in reverse Simple'Range loop
         if Simple (Pos) = '.' then
            return Simple (1 .. Pos - 1);
         end if;
      end loop;

      --  If there is no dot, return the complete file name

      return Simple;
   end Base_Name;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : Dir_Type_Value) is
      Discard : Integer;
      pragma Warnings (Off, Discard);

      function closedir (directory : DIRs) return Integer;
      pragma Import (C, closedir, "__gnat_closedir");

   begin
      Discard := closedir (DIRs (Dir));
   end Close;

   -------------
   -- Compose --
   -------------

   function Compose
     (Containing_Directory : String := "";
      Name                 : String;
      Extension            : String := "") return String
   is
      Result : String (1 .. Containing_Directory'Length +
                              Name'Length + Extension'Length + 2);
      Last   : Natural;

   begin
      --  First, deal with the invalid cases

      if Containing_Directory /= ""
        and then not Is_Valid_Path_Name (Containing_Directory)
      then
         raise Name_Error with
           "invalid directory path name """ & Containing_Directory & '"';

      elsif
        Extension'Length = 0 and then (not Is_Valid_Simple_Name (Name))
      then
         raise Name_Error with
           "invalid simple name """ & Name & '"';

      elsif Extension'Length /= 0
        and then not Is_Valid_Simple_Name (Name & '.' & Extension)
      then
         raise Name_Error with
           "invalid file name """ & Name & '.' & Extension & '"';

      --  This is not an invalid case so build the path name

      else
         Last := Containing_Directory'Length;
         Result (1 .. Last) := Containing_Directory;

         --  Add a directory separator if needed

         if Last /= 0 and then Result (Last) /= Dir_Separator then
            Last := Last + 1;
            Result (Last) := Dir_Separator;
         end if;

         --  Add the file name

         Result (Last + 1 .. Last + Name'Length) := Name;
         Last := Last + Name'Length;

         --  If extension was specified, add dot followed by this extension

         if Extension'Length /= 0 then
            Last := Last + 1;
            Result (Last) := '.';
            Result (Last + 1 .. Last + Extension'Length) := Extension;
            Last := Last + Extension'Length;
         end if;

         return Result (1 .. Last);
      end if;
   end Compose;

   --------------------------
   -- Containing_Directory --
   --------------------------

   function Containing_Directory (Name : String) return String is
   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';

      else
         declare
            --  We need to resolve links because of A.16(47), since we must not
            --  return alternative names for files.

            Norm    : constant String := Normalize_Pathname (Name);
            Last_DS : constant Natural :=
                        Strings.Fixed.Index
                          (Name, Dir_Seps, Going => Strings.Backward);

         begin
            if Last_DS = 0 then

               --  There is no directory separator, returns current working
               --  directory.

               return Current_Directory;

            --  If Name indicates a root directory, raise Use_Error, because
            --  it has no containing directory.

            elsif Norm = "/"
              or else
                (Windows
                 and then
                   (Norm = "\"
                    or else
                      (Norm'Length = 3
                        and then Norm (Norm'Last - 1 .. Norm'Last) = ":\"
                        and then (Norm (Norm'First) in 'a' .. 'z'
                                   or else Norm (Norm'First) in 'A' .. 'Z'))))
            then
               raise Use_Error with
                 "directory """ & Name & """ has no containing directory";

            else
               declare
                  Last   : Positive := Last_DS - Name'First + 1;
                  Result : String (1 .. Last);

               begin
                  Result := Name (Name'First .. Last_DS);

                  --  Remove any trailing directory separator, except as the
                  --  first character or the first character following a drive
                  --  number on Windows.

                  while Last > 1 loop
                     exit when
                       Result (Last) /= '/'
                         and then
                       Result (Last) /= Directory_Separator;

                     exit when Windows
                       and then Last = 3
                       and then Result (2) = ':'
                       and then
                         (Result (1) in 'A' .. 'Z'
                           or else
                          Result (1) in 'a' .. 'z');

                     Last := Last - 1;
                  end loop;

                  --  Special case of current directory, identified by "."

                  if Last = 1 and then Result (1) = '.' then
                     return Current_Directory;

                  --  Special case of "..": the current directory may be a root
                  --  directory.

                  elsif Last = 2 and then Result (1 .. 2) = ".." then
                     return Containing_Directory (Current_Directory);

                  else
                     return Result (1 .. Last);
                  end if;
               end;
            end if;
         end;
      end if;
   end Containing_Directory;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Source_Name : String;
      Target_Name : String;
      Form        : String := "")
   is
      pragma Unreferenced (Form);
      Success : Boolean;

   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Source_Name) then
         raise Name_Error with
           "invalid source path name """ & Source_Name & '"';

      elsif not Is_Valid_Path_Name (Target_Name) then
         raise Name_Error with
           "invalid target path name """ & Target_Name & '"';

      elsif not Is_Regular_File (Source_Name) then
         raise Name_Error with '"' & Source_Name & """ is not a file";

      elsif Is_Directory (Target_Name) then
         raise Use_Error with "target """ & Target_Name & """ is a directory";

      else
         --  The implementation uses System.OS_Lib.Copy_File, with parameters
         --  suitable for all platforms.

         Copy_File (Source_Name, Target_Name, Success, Overwrite, None);

         if not Success then
            raise Use_Error with "copy of """ & Source_Name & """ failed";
         end if;
      end if;
   end Copy_File;

   ----------------------
   -- Create_Directory --
   ----------------------

   procedure Create_Directory
     (New_Directory : String;
      Form          : String := "")
   is
      pragma Unreferenced (Form);

      C_Dir_Name : constant String := New_Directory & ASCII.NUL;

      function mkdir (Dir_Name : String) return Integer;
      pragma Import (C, mkdir, "__gnat_mkdir");

   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (New_Directory) then
         raise Name_Error with
           "invalid new directory path name """ & New_Directory & '"';

      else
         if mkdir (C_Dir_Name) /= 0 then
            raise Use_Error with
              "creation of new directory """ & New_Directory & """ failed";
         end if;
      end if;
   end Create_Directory;

   -----------------
   -- Create_Path --
   -----------------

   procedure Create_Path
     (New_Directory : String;
      Form          : String := "")
   is
      pragma Unreferenced (Form);

      New_Dir : String (1 .. New_Directory'Length + 1);
      Last    : Positive := 1;

   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (New_Directory) then
         raise Name_Error with
           "invalid new directory path name """ & New_Directory & '"';

      else
         --  Build New_Dir with a directory separator at the end, so that the
         --  complete path will be found in the loop below.

         New_Dir (1 .. New_Directory'Length) := New_Directory;
         New_Dir (New_Dir'Last) := Directory_Separator;

         --  Create, if necessary, each directory in the path

         for J in 2 .. New_Dir'Last loop

            --  Look for the end of an intermediate directory

            if New_Dir (J) /= Dir_Separator and then
               New_Dir (J) /= '/'
            then
               Last := J;

            --  We have found a new intermediate directory each time we find
            --  a first directory separator.

            elsif New_Dir (J - 1) /= Dir_Separator and then
                  New_Dir (J - 1) /= '/'
            then

               --  No need to create the directory if it already exists

               if Is_Directory (New_Dir (1 .. Last)) then
                  null;

               --  It is an error if a file with such a name already exists

               elsif Is_Regular_File (New_Dir (1 .. Last)) then
                  raise Use_Error with
                    "file """ & New_Dir (1 .. Last) & """ already exists";

               else
                  Create_Directory (New_Directory => New_Dir (1 .. Last));
               end if;
            end if;
         end loop;
      end if;
   end Create_Path;

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return String is
      Path_Len : Natural := Max_Path;
      Buffer   : String (1 .. 1 + Max_Path + 1);

      procedure Local_Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Local_Get_Current_Dir, "__gnat_get_current_dir");

   begin
      Local_Get_Current_Dir (Buffer'Address, Path_Len'Address);

      declare
         --  We need to resolve links because of A.16(47), since we must not
         --  return alternative names for files
         Cur : constant String := Normalize_Pathname (Buffer (1 .. Path_Len));

      begin
         if Cur'Length > 1 and then Cur (Cur'Last) = Dir_Separator then
            return Cur (1 .. Cur'Last - 1);
         else
            return Cur;
         end if;
      end;
   end Current_Directory;

   ----------------------
   -- Delete_Directory --
   ----------------------

   procedure Delete_Directory (Directory : String) is
   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Directory) then
         raise Name_Error with
           "invalid directory path name """ & Directory & '"';

      elsif not Is_Directory (Directory) then
         raise Name_Error with '"' & Directory & """ not a directory";

      else
         declare
            C_Dir_Name : constant String := Directory & ASCII.NUL;

         begin
            if rmdir (C_Dir_Name) /= 0 then
               raise Use_Error with
                 "deletion of directory """ & Directory & """ failed";
            end if;
         end;
      end if;
   end Delete_Directory;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : String) is
      Success : Boolean;

   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';

      elsif not Is_Regular_File (Name) then
         raise Name_Error with "file """ & Name & """ does not exist";

      else
         --  The implementation uses System.OS_Lib.Delete_File

         Delete_File (Name, Success);

         if not Success then
            raise Use_Error with "file """ & Name & """ could not be deleted";
         end if;
      end if;
   end Delete_File;

   -----------------
   -- Delete_Tree --
   -----------------

   procedure Delete_Tree (Directory : String) is
      Current_Dir : constant String := Current_Directory;
      Search      : Search_Type;
      Dir_Ent     : Directory_Entry_Type;
   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Directory) then
         raise Name_Error with
           "invalid directory path name """ & Directory & '"';

      elsif not Is_Directory (Directory) then
         raise Name_Error with '"' & Directory & """ not a directory";

      else
         Set_Directory (Directory);
         Start_Search (Search, Directory => ".", Pattern => "");

         while More_Entries (Search) loop
            Get_Next_Entry (Search, Dir_Ent);

            declare
               File_Name : constant String := Simple_Name (Dir_Ent);

            begin
               if System.OS_Lib.Is_Directory (File_Name) then
                  if File_Name /= "." and then File_Name /= ".." then
                     Delete_Tree (File_Name);
                  end if;

               else
                  Delete_File (File_Name);
               end if;
            end;
         end loop;

         Set_Directory (Current_Dir);
         End_Search (Search);

         declare
            C_Dir_Name : constant String := Directory & ASCII.NUL;

         begin
            if rmdir (C_Dir_Name) /= 0 then
               raise Use_Error with
                 "directory tree rooted at """ &
                   Directory & """ could not be deleted";
            end if;
         end;
      end if;
   end Delete_Tree;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';

      else
         --  The implementation is in File_Exists

         return File_Exists (Name);
      end if;
   end Exists;

   ---------------
   -- Extension --
   ---------------

   function Extension (Name : String) return String is
   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';

      else
         --  Look for first dot that is not followed by a directory separator

         for Pos in reverse Name'Range loop

            --  If a directory separator is found before a dot, there is no
            --  extension.

            if Name (Pos) = Dir_Separator then
               return Empty_String;

            elsif Name (Pos) = '.' then

               --  We found a dot, build the return value with lower bound 1

               declare
                  subtype Result_Type is String (1 .. Name'Last - Pos);
               begin
                  return Result_Type (Name (Pos + 1 .. Name'Last));
               end;
            end if;
         end loop;

         --  No dot were found, there is no extension

         return Empty_String;
      end if;
   end Extension;

   ----------------------
   -- Fetch_Next_Entry --
   ----------------------

   procedure Fetch_Next_Entry (Search : Search_Type) is
      Name : String (1 .. 255);
      Last : Natural;

      Kind : File_Kind := Ordinary_File;
      --  Initialized to avoid a compilation warning

      Filename_Addr : System.Address;
      Filename_Len  : aliased Integer;

      Buffer : array (0 .. Filename_Max + 12) of Character;
      --  12 is the size of the dirent structure (see dirent.h), without the
      --  field for the filename.

      function readdir_gnat
        (Directory : System.Address;
         Buffer    : System.Address;
         Last      : not null access Integer) return System.Address;
      pragma Import (C, readdir_gnat, "__gnat_readdir");

      use System;

   begin
      --  Search.Value.Is_Valid is always True when Fetch_Next_Entry is called

      loop
         Filename_Addr :=
           readdir_gnat
             (System.Address (Search.Value.Dir),
              Buffer'Address,
              Filename_Len'Access);

         --  If no matching entry is found, set Is_Valid to False

         if Filename_Addr = System.Null_Address then
            Search.Value.Is_Valid := False;
            exit;
         end if;

         declare
            subtype Path_String is String (1 .. Filename_Len);
            type    Path_String_Access is access Path_String;

            function Address_To_Access is new
              Ada.Unchecked_Conversion
                (Source => Address,
                 Target => Path_String_Access);

            Path_Access : constant Path_String_Access :=
                            Address_To_Access (Filename_Addr);

         begin
            Last := Filename_Len;
            Name (1 .. Last) := Path_Access.all;
         end;

         --  Check if the entry matches the pattern

         if Match (Name (1 .. Last), Search.Value.Pattern) then
            declare
               Full_Name : constant String :=
                             Compose
                               (To_String
                                  (Search.Value.Name), Name (1 .. Last));
               Found     : Boolean := False;

            begin
               if File_Exists (Full_Name) then

                  --  Now check if the file kind matches the filter

                  if Is_Regular_File (Full_Name) then
                     if Search.Value.Filter (Ordinary_File) then
                        Kind := Ordinary_File;
                        Found := True;
                     end if;

                  elsif Is_Directory (Full_Name) then
                     if Search.Value.Filter (Directory) then
                        Kind := Directory;
                        Found := True;
                     end if;

                  elsif Search.Value.Filter (Special_File) then
                     Kind := Special_File;
                     Found := True;
                  end if;

                  --  If it does, update Search and return

                  if Found then
                     Search.Value.Entry_Fetched := True;
                     Search.Value.Dir_Entry :=
                       (Is_Valid => True,
                        Simple   => To_Unbounded_String (Name (1 .. Last)),
                        Full     => To_Unbounded_String (Full_Name),
                        Kind     => Kind);
                     exit;
                  end if;
               end if;
            end;
         end if;
      end loop;
   end Fetch_Next_Entry;

   -----------------
   -- File_Exists --
   -----------------

   function File_Exists (Name : String) return Boolean is
      function C_File_Exists (A : System.Address) return Integer;
      pragma Import (C, C_File_Exists, "__gnat_file_exists");

      C_Name : String (1 .. Name'Length + 1);

   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      return C_File_Exists (C_Name (1)'Address) = 1;
   end File_Exists;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Search : in out Search_Type) is
   begin
      if Search.Value /= null then

         --  Close the directory, if one is open

         if Search.Value.Dir /= No_Dir then
            Close (Search.Value.Dir);
         end if;

         Free (Search.Value);
      end if;
   end Finalize;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name (Name : String) return String is
   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';

      else
         --  Build the return value with lower bound 1

         --  Use System.OS_Lib.Normalize_Pathname

         declare
            --  We need to resolve links because of A.16(47), since we must not
            --  return alternative names for files
            Value : constant String := Normalize_Pathname (Name);
            subtype Result is String (1 .. Value'Length);
         begin
            return Result (Value);
         end;
      end if;
   end Full_Name;

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error with "invalid directory entry";

      else
         --  The value to return has already been computed

         return To_String (Directory_Entry.Full);
      end if;
   end Full_Name;

   --------------------
   -- Get_Next_Entry --
   --------------------

   procedure Get_Next_Entry
     (Search          : in out Search_Type;
      Directory_Entry : out Directory_Entry_Type)
   is
   begin
      --  First, the invalid case

      if Search.Value = null or else not Search.Value.Is_Valid then
         raise Status_Error with "invalid search";
      end if;

      --  Fetch the next entry, if needed

      if not Search.Value.Entry_Fetched then
         Fetch_Next_Entry (Search);
      end if;

      --  It is an error if no valid entry is found

      if not Search.Value.Is_Valid then
         raise Status_Error with "no next entry";

      else
         --  Reset Entry_Fetched and return the entry

         Search.Value.Entry_Fetched := False;
         Directory_Entry := Search.Value.Dir_Entry;
      end if;
   end Get_Next_Entry;

   ----------
   -- Kind --
   ----------

   function Kind (Name : String) return File_Kind is
   begin
      --  First, the invalid case

      if not File_Exists (Name) then
         raise Name_Error with "file """ & Name & """ does not exist";

      elsif Is_Regular_File (Name) then
         return Ordinary_File;

      elsif Is_Directory (Name) then
         return Directory;

      else
         return Special_File;
      end if;
   end Kind;

   function Kind (Directory_Entry : Directory_Entry_Type) return File_Kind is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error with "invalid directory entry";

      else
         --  The value to return has already be computed

         return Directory_Entry.Kind;
      end if;
   end Kind;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time (Name : String) return Time is
      Date   : OS_Time;
      Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type;
      Result : Time;

   begin
      --  First, the invalid cases

      if not (Is_Regular_File (Name) or else Is_Directory (Name)) then
         raise Name_Error with '"' & Name & """ not a file or directory";

      else
         Date := File_Time_Stamp (Name);

         --  Break down the time stamp into its constituents relative to GMT.
         --  This version of Split does not recognize leap seconds or buffer
         --  space for time zone processing.

         GM_Split (Date, Year, Month, Day, Hour, Minute, Second);

         --  On OpenVMS, the resulting time value must be in the local time
         --  zone. Ada.Calendar.Time_Of is exactly what we need. Note that
         --  in both cases, the sub seconds are set to zero (0.0) because the
         --  time stamp does not store them in its value.

         if OpenVMS then
            Result :=
              Ada.Calendar.Time_Of
                (Year, Month, Day, Seconds_Of (Hour, Minute, Second, 0.0));

         --  On Unix and Windows, the result must be in GMT. Ada.Calendar.
         --  Formatting.Time_Of with default time zone of zero (0) is the
         --  routine of choice.

         else
            Result := Time_Of (Year, Month, Day, Hour, Minute, Second, 0.0);
         end if;

         return Result;
      end if;
   end Modification_Time;

   function Modification_Time
     (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time
   is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error with "invalid directory entry";

      else
         --  The value to return has already be computed

         return Modification_Time (To_String (Directory_Entry.Full));
      end if;
   end Modification_Time;

   ------------------
   -- More_Entries --
   ------------------

   function More_Entries (Search : Search_Type) return Boolean is
   begin
      if Search.Value = null then
         return False;

      elsif Search.Value.Is_Valid then

         --  Fetch the next entry, if needed

         if not Search.Value.Entry_Fetched then
            Fetch_Next_Entry (Search);
         end if;
      end if;

      return Search.Value.Is_Valid;
   end More_Entries;

   ------------
   -- Rename --
   ------------

   procedure Rename (Old_Name, New_Name : String) is
      Success : Boolean;

   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Old_Name) then
         raise Name_Error with "invalid old path name """ & Old_Name & '"';

      elsif not Is_Valid_Path_Name (New_Name) then
         raise Name_Error with "invalid new path name """ & New_Name & '"';

      elsif not Is_Regular_File (Old_Name)
            and then not Is_Directory (Old_Name)
      then
         raise Name_Error with "old file """ & Old_Name & """ does not exist";

      elsif Is_Regular_File (New_Name) or else Is_Directory (New_Name) then
         raise Use_Error with
           "new name """ & New_Name
           & """ designates a file that already exists";

      else
         --  The implementation uses System.OS_Lib.Rename_File

         Rename_File (Old_Name, New_Name, Success);

         if not Success then
            raise Use_Error with
              "file """ & Old_Name & """ could not be renamed";
         end if;
      end if;
   end Rename;

   ------------
   -- Search --
   ------------

   procedure Search
     (Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True);
      Process   : not null access procedure
                                    (Directory_Entry : Directory_Entry_Type))
   is
      Srch            : Search_Type;
      Directory_Entry : Directory_Entry_Type;

   begin
      Start_Search (Srch, Directory, Pattern, Filter);

      while More_Entries (Srch) loop
         Get_Next_Entry (Srch, Directory_Entry);
         Process (Directory_Entry);
      end loop;

      End_Search (Srch);
   end Search;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory (Directory : String) is
      C_Dir_Name : constant String := Directory & ASCII.NUL;
   begin
      if not Is_Valid_Path_Name (Directory) then
         raise Name_Error with
           "invalid directory path name & """ & Directory & '"';

      elsif not Is_Directory (Directory) then
         raise Name_Error with
           "directory """ & Directory & """ does not exist";

      elsif chdir (C_Dir_Name) /= 0 then
         raise Name_Error with
           "could not set to designated directory """ & Directory & '"';
      end if;
   end Set_Directory;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Name : String) return String is

      function Simple_Name_Internal (Path : String) return String;
      --  This function does the job

      --------------------------
      -- Simple_Name_Internal --
      --------------------------

      function Simple_Name_Internal (Path : String) return String is
         Cut_Start : Natural :=
                       Strings.Fixed.Index
                         (Path, Dir_Seps, Going => Strings.Backward);
         Cut_End   : Natural;

      begin
         --  Cut_Start pointS to the first simple name character

         Cut_Start := (if Cut_Start = 0 then Path'First else Cut_Start + 1);

         --  Cut_End point to the last simple name character

         Cut_End := Path'Last;

         Check_For_Standard_Dirs : declare
            BN               : constant String := Path (Cut_Start .. Cut_End);
            Has_Drive_Letter : constant Boolean :=
                                 System.OS_Lib.Path_Separator /= ':';
            --  If Path separator is not ':' then we are on a DOS based OS
            --  where this character is used as a drive letter separator.

         begin
            if BN = "." or else BN = ".." then
               return "";

            elsif Has_Drive_Letter
              and then BN'Length > 2
              and then Characters.Handling.Is_Letter (BN (BN'First))
              and then BN (BN'First + 1) = ':'
            then
               --  We have a DOS drive letter prefix, remove it

               return BN (BN'First + 2 .. BN'Last);

            else
               return BN;
            end if;
         end Check_For_Standard_Dirs;
      end Simple_Name_Internal;

   --  Start of processing for Simple_Name

   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error with "invalid path name """ & Name & '"';

      else
         --  Build the value to return with lower bound 1

         declare
            Value : constant String := Simple_Name_Internal (Name);
            subtype Result is String (1 .. Value'Length);
         begin
            return Result (Value);
         end;
      end if;
   end Simple_Name;

   function Simple_Name
     (Directory_Entry : Directory_Entry_Type) return String is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error with "invalid directory entry";

      else
         --  The value to return has already be computed

         return To_String (Directory_Entry.Simple);
      end if;
   end Simple_Name;

   ----------
   -- Size --
   ----------

   function Size (Name : String) return File_Size is
      C_Name : String (1 .. Name'Length + 1);

      function C_Size (Name : System.Address) return Long_Integer;
      pragma Import (C, C_Size, "__gnat_named_file_length");

   begin
      --  First, the invalid case

      if not Is_Regular_File (Name) then
         raise Name_Error with "file """ & Name & """ does not exist";

      else
         C_Name (1 .. Name'Length) := Name;
         C_Name (C_Name'Last) := ASCII.NUL;
         return File_Size (C_Size (C_Name'Address));
      end if;
   end Size;

   function Size (Directory_Entry : Directory_Entry_Type) return File_Size is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error with "invalid directory entry";

      else
         --  The value to return has already be computed

         return Size (To_String (Directory_Entry.Full));
      end if;
   end Size;

   ------------------
   -- Start_Search --
   ------------------

   procedure Start_Search
     (Search    : in out Search_Type;
      Directory : String;
      Pattern   : String;
      Filter    : Filter_Type := (others => True))
   is
      function opendir (file_name : String) return DIRs;
      pragma Import (C, opendir, "__gnat_opendir");

      C_File_Name : constant String := Directory & ASCII.NUL;
      Pat         : Regexp;
      Dir         : Dir_Type_Value;

   begin
      --  First, the invalid case Name_Error

      if not Is_Directory (Directory) then
         raise Name_Error with
           "unknown directory """ & Simple_Name (Directory) & '"';
      end if;

      --  Check the pattern

      begin
         Pat := Compile
           (Pattern,
            Glob           => True,
            Case_Sensitive => Is_Path_Name_Case_Sensitive);
      exception
         when Error_In_Regexp =>
            Free (Search.Value);
            raise Name_Error with "invalid pattern """ & Pattern & '"';
      end;

      Dir := Dir_Type_Value (opendir (C_File_Name));

      if Dir = No_Dir then
         raise Use_Error with
           "unreadable directory """ & Simple_Name (Directory) & '"';
      end if;

      --  If needed, finalize Search

      Finalize (Search);

      --  Allocate the default data

      Search.Value := new Search_Data;

      --  Initialize some Search components

      Search.Value.Filter   := Filter;
      Search.Value.Name     := To_Unbounded_String (Full_Name (Directory));
      Search.Value.Pattern  := Pat;
      Search.Value.Dir      := Dir;
      Search.Value.Is_Valid := True;
   end Start_Search;

end Ada.Directories;
