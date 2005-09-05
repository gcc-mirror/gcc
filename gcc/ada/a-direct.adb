------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      A D A . D I R E C T O R I E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories.Validity;   use Ada.Directories.Validity;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;    use Ada.Characters.Handling;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regexp;                use GNAT.Regexp;
--  ??? Ada units should not depend on GNAT units

with System;

package body Ada.Directories is

   type Search_Data is record
      Is_Valid      : Boolean := False;
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Pattern       : Regexp;
      Filter        : Filter_Type;
      Dir           : Dir_Type;
      Entry_Fetched : Boolean := False;
      Dir_Entry     : Directory_Entry_Type;
   end record;
   --  Comment required ???

   Empty_String : constant String := (1 .. 0 => ASCII.NUL);
   --  Comment required ???

   procedure Free is new Ada.Unchecked_Deallocation (Search_Data, Search_Ptr);

   function File_Exists (Name : String) return Boolean;
   --  Returns True if the named file exists

   procedure Fetch_Next_Entry (Search : Search_Type);
   --  Get the next entry in a directory, setting Entry_Fetched if successful
   --  or resetting Is_Valid if not.

   procedure To_Lower_If_Case_Insensitive (S : in out String);
   --  Put S in lower case if file and path names are case-insensitive

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Name : String) return String is
      Simple : String := Simple_Name (Name);
      --  Simple'First is guaranteed to be 1

   begin
      To_Lower_If_Case_Insensitive (Simple);

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

      if not Is_Valid_Path_Name (Containing_Directory) then
         raise Name_Error;

      elsif
        Extension'Length = 0 and then (not Is_Valid_Simple_Name (Name))
      then
         raise Name_Error;

      elsif Extension'Length /= 0 and then
        (not Is_Valid_Simple_Name (Name & '.' & Extension))
      then
         raise Name_Error;

         --  This is not an invalid case so build the path name

      else
         Last := Containing_Directory'Length;
         Result (1 .. Last) := Containing_Directory;

         --  Add a directory separator if needed

         if Result (Last) /= Dir_Separator then
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

         To_Lower_If_Case_Insensitive (Result (1 .. Last));
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
         raise Name_Error;

      else
         --  Get the directory name using GNAT.Directory_Operations.Dir_Name

         declare
            Value : constant String := Dir_Name (Path => Name);
            Result : String (1 .. Value'Length);
            Last : Natural := Result'Last;

         begin
            Result := Value;

            --  Remove any trailing directory separator, except as the first
            --  character.

            while Last > 1 and then Result (Last) = Dir_Separator loop
               Last := Last - 1;
            end loop;

            --  Special case of current directory, identified by "."

            if Last = 1 and then Result (1) = '.' then
               return Get_Current_Dir;

            else
               To_Lower_If_Case_Insensitive (Result (1 .. Last));
               return Result (1 .. Last);
            end if;
         end;
      end if;
   end Containing_Directory;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Source_Name   : String;
      Target_Name   : String;
      Form          : String := "")
   is
      pragma Unreferenced (Form);
      Success : Boolean;

   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Source_Name)
        or else not Is_Valid_Path_Name (Target_Name)
        or else not Is_Regular_File (Source_Name)
      then
         raise Name_Error;

      elsif Is_Directory (Target_Name) then
         raise Use_Error;

      else
         --  The implementation uses GNAT.OS_Lib.Copy_File, with parameters
         --  suitable for all platforms.

         Copy_File
           (Source_Name, Target_Name, Success, Overwrite, None);

         if not Success then
            raise Use_Error;
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

   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (New_Directory) then
         raise Name_Error;

      else
         --  The implementation uses GNAT.Directory_Operations.Make_Dir

         begin
            Make_Dir (Dir_Name => New_Directory);

         exception
            when Directory_Error =>
               raise Use_Error;
         end;
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
         raise Name_Error;

      else
         --  Build New_Dir with a directory separator at the end, so that the
         --  complete path will be found in the loop below.

         New_Dir (1 .. New_Directory'Length) := New_Directory;
         New_Dir (New_Dir'Last) := Directory_Separator;

         --  Create, if necessary, each directory in the path

         for J in 2 .. New_Dir'Last loop

            --  Look for the end of an intermediate directory

            if New_Dir (J) /= Dir_Separator then
               Last := J;

            --  We have found a new intermediate directory each time we find
            --  a first directory separator.

            elsif New_Dir (J - 1) /= Dir_Separator then

               --  No need to create the directory if it already exists

               if Is_Directory (New_Dir (1 .. Last)) then
                  null;

               --  It is an error if a file with such a name already exists

               elsif Is_Regular_File (New_Dir (1 .. Last)) then
                  raise Use_Error;

               else
                  --  The implementation uses
                  --  GNAT.Directory_Operations.Make_Dir.

                  begin
                     Make_Dir (Dir_Name => New_Dir (1 .. Last));

                  exception
                     when Directory_Error =>
                        raise Use_Error;
                  end;
               end if;
            end if;
         end loop;
      end if;
   end Create_Path;

   -----------------------
   -- Current_Directory --
   -----------------------

   function Current_Directory return String is

      --  The implementation uses GNAT.Directory_Operations.Get_Current_Dir

      Cur : String := Normalize_Pathname (Get_Current_Dir);

   begin
      To_Lower_If_Case_Insensitive (Cur);

      if Cur'Length > 1 and then Cur (Cur'Last) = Dir_Separator then
         return Cur (1 .. Cur'Last - 1);
      else
         return Cur;
      end if;
   end Current_Directory;

   ----------------------
   -- Delete_Directory --
   ----------------------

   procedure Delete_Directory (Directory : String) is
   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Directory) then
         raise Name_Error;

      elsif not Is_Directory (Directory) then
         raise Name_Error;

      else
         --  The implementation uses GNAT.Directory_Operations.Remove_Dir

         begin
            Remove_Dir (Dir_Name => Directory, Recursive => False);

         exception
            when Directory_Error =>
               raise Use_Error;
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
         raise Name_Error;

      elsif not Is_Regular_File (Name) then
         raise Name_Error;

      else
         --  The implementation uses GNAT.OS_Lib.Delete_File

         Delete_File (Name, Success);

         if not Success then
            raise Use_Error;
         end if;
      end if;
   end Delete_File;

   -----------------
   -- Delete_Tree --
   -----------------

   procedure Delete_Tree (Directory : String) is
   begin
      --  First, the invalid cases

      if not Is_Valid_Path_Name (Directory) then
         raise Name_Error;

      elsif not Is_Directory (Directory) then
         raise Name_Error;

      else
         --  The implementation uses GNAT.Directory_Operations.Remove_Dir

         begin
            Remove_Dir (Directory, Recursive => True);

         exception
            when Directory_Error =>
               raise Use_Error;
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
         raise Name_Error;

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
         raise Name_Error;

      else
         --  Look for first dot that is not followed by a directory separator

         for Pos in reverse Name'Range loop

            --  If a directory separator is found before a dot, there
            --  is no extension.

            if Name (Pos) = Dir_Separator then
               return Empty_String;

            elsif Name (Pos) = '.' then

               --  We found a dot, build the return value with lower bound 1

               declare
                  Result : String (1 .. Name'Last - Pos);
               begin
                  Result := Name (Pos + 1 .. Name'Last);
                  return Result;
                  --  This should be done with a subtype conversion, avoiding
                  --  the unnecessary junk copy ???
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

   begin
      --  Search.Value.Is_Valid is always True when Fetch_Next_Entry is called

      loop
         Read (Search.Value.Dir, Name, Last);

         --  If no matching entry is found, set Is_Valid to False

         if Last = 0 then
            Search.Value.Is_Valid := False;
            exit;
         end if;

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

         if Is_Open (Search.Value.Dir) then
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
         raise Name_Error;

      else
         --  Build the return value with lower bound 1

         --  Use GNAT.OS_Lib.Normalize_Pathname

         declare
            Value : String := Normalize_Pathname (Name);
            subtype Result is String (1 .. Value'Length);
         begin
            To_Lower_If_Case_Insensitive (Value);
            return Result (Value);
         end;
      end if;
   end Full_Name;

   function Full_Name (Directory_Entry : Directory_Entry_Type) return String is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error;

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
         raise Status_Error;
      end if;

      --  Fetch the next entry, if needed

      if not Search.Value.Entry_Fetched then
         Fetch_Next_Entry (Search);
      end if;

      --  It is an error if no valid entry is found

      if not Search.Value.Is_Valid then
         raise Status_Error;

      else
         --  Reset Entry_Fatched and return the entry

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
         raise Name_Error;

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
         raise Status_Error;

      else
         --  The value to return has already be computed

         return Directory_Entry.Kind;
      end if;
   end Kind;

   -----------------------
   -- Modification_Time --
   -----------------------

   function Modification_Time (Name : String) return Ada.Calendar.Time is
      Date   : OS_Time;
      Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type;

   begin
      --  First, the invalid cases

      if not (Is_Regular_File (Name) or else Is_Directory (Name)) then
         raise Name_Error;

      else
         Date := File_Time_Stamp (Name);
         --  ???? We need to be able to convert OS_Time to Ada.Calendar.Time
         --  For now, use the component of the OS_Time to create the
         --  Calendar.Time value.

         GM_Split (Date, Year, Month, Day, Hour, Minute, Second);

         return Ada.Calendar.Time_Of
           (Year, Month, Day, Duration (Second + 60 * (Minute + 60 * Hour)));
      end if;
   end Modification_Time;

   function Modification_Time
     (Directory_Entry : Directory_Entry_Type) return Ada.Calendar.Time
   is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error;

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

      if not Is_Valid_Path_Name (Old_Name)
        or else not Is_Valid_Path_Name (New_Name)
        or else (not Is_Regular_File (Old_Name)
                   and then not Is_Directory (Old_Name))
      then
         raise Name_Error;

      elsif Is_Regular_File (New_Name) or Is_Directory (New_Name) then
         raise Use_Error;

      else
         --  The implementation uses GNAT.OS_Lib.Rename_File

         Rename_File (Old_Name, New_Name, Success);

         if not Success then
            raise Use_Error;
         end if;
      end if;
   end Rename;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory (Directory : String) is
   begin
      --  The implementation uses GNAT.Directory_Operations.Change_Dir

      Change_Dir (Dir_Name => Directory);

   exception
      when Directory_Error =>
         raise Name_Error;
   end Set_Directory;

   -----------------
   -- Simple_Name --
   -----------------

   function Simple_Name (Name : String) return String is
   begin
      --  First, the invalid case

      if not Is_Valid_Path_Name (Name) then
         raise Name_Error;

      else
         --  Build the value to return with lower bound 1

         --  The implementation uses GNAT.Directory_Operations.Base_Name

         declare
            Value  : String := GNAT.Directory_Operations.Base_Name (Name);
            subtype Result is String (1 .. Value'Length);
         begin
            To_Lower_If_Case_Insensitive (Value);
            return Result (Value);
         end;
      end if;
   end Simple_Name;

   function Simple_Name
     (Directory_Entry : Directory_Entry_Type) return String
   is
   begin
      --  First, the invalid case

      if not Directory_Entry.Is_Valid then
         raise Status_Error;

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
         raise Name_Error;

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
         raise Status_Error;

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
   begin
      --  First, the invalid case

      if not Is_Directory (Directory) then
         raise Name_Error;
      end if;

      --  If needed, finalize Search

      Finalize (Search);

      --  Allocate the default data

      Search.Value := new Search_Data;

      begin
         --  Check the pattern

         Search.Value.Pattern := Compile (Pattern, Glob => True);

      exception
         when Error_In_Regexp =>
            Free (Search.Value);
            raise Name_Error;
      end;

      --  Initialize some Search components

      Search.Value.Filter := Filter;
      Search.Value.Name := To_Unbounded_String (Full_Name (Directory));
      Open (Search.Value.Dir, Directory);
      Search.Value.Is_Valid := True;
   end Start_Search;

   ----------------------------------
   -- To_Lower_If_Case_Insensitive --
   ----------------------------------

   procedure To_Lower_If_Case_Insensitive (S : in out String) is
   begin
      if not Is_Path_Name_Case_Sensitive then
         for J in S'Range loop
            S (J) := To_Lower (S (J));
         end loop;
      end if;
   end To_Lower_If_Case_Insensitive;

end Ada.Directories;
