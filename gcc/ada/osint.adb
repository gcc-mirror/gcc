------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Alloc;
with Debug;
with Fmap;     use Fmap;
with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Opt;      use Opt;
with Output;   use Output;
with Sdefault; use Sdefault;
with Table;
with Targparm; use Targparm;

with Ada.Unchecked_Conversion;

pragma Warnings (Off);
--  This package is used also by gnatcoll
with System.Case_Util; use System.Case_Util;
with System.CRTL;
pragma Warnings (On);

with GNAT.HTable;

package body Osint is

   use type CRTL.size_t;

   Running_Program : Program_Type := Unspecified;
   --  Set by Set_Program to indicate which of Compiler, Binder, etc is
   --  running.

   Program_Set : Boolean := False;
   --  True if Set_Program has been called; used to detect duplicate calls.

   Std_Prefix : String_Ptr;
   --  Standard prefix, computed dynamically the first time Relocate_Path
   --  is called, and cached for subsequent calls.

   Empty  : aliased String := "";
   No_Dir : constant String_Ptr := Empty'Access;
   --  Used in Locate_File as a fake directory when Name is already an
   --  absolute path.

   -------------------------------------
   -- Use of Name_Find and Name_Enter --
   -------------------------------------

   --  This package creates a number of source, ALI and object file names
   --  that are used to locate the actual file and for the purpose of message
   --  construction. These names need not be accessible by Name_Find, and can
   --  be therefore created by using routine Name_Enter. The files in question
   --  are file names with a prefix directory (i.e., the files not in the
   --  current directory). File names without a prefix directory are entered
   --  with Name_Find because special values might be attached to the various
   --  Info fields of the corresponding name table entry.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Append_Suffix_To_File_Name
     (Name   : File_Name_Type;
      Suffix : String) return File_Name_Type;
   --  Appends Suffix to Name and returns the new name

   function OS_Time_To_GNAT_Time (T : OS_Time) return Time_Stamp_Type;
   --  Convert OS format time to GNAT format time stamp. If T is Invalid_Time,
   --  then returns Empty_Time_Stamp.
   --  Round to even seconds on Windows before conversion.
   --  Windows ALI files had timestamps rounded to even seconds historically.
   --  The rounding was originally done in GM_Split. Now that GM_Split no
   --  longer does it, we are rounding it here only for ALI files.

   function Executable_Prefix return String_Ptr;
   --  Returns the name of the root directory where the executable is stored.
   --  The executable must be located in a directory called "bin", or under
   --  root/lib/gcc-lib/..., or under root/libexec/gcc/... For example, if
   --  executable is stored in directory "/foo/bar/bin", this routine returns
   --  "/foo/bar/". Return "" if location is not recognized as described above.

   function File_Names_Equal (File1, File2 : String) return Boolean;
   --  Compare File1 and File2 taking into account the case insensitivity
   --  of the OS.

   function Update_Path (Path : String_Ptr) return String_Ptr;
   --  Update the specified path to replace the prefix with the location where
   --  GNAT is installed. See the file prefix.c in GCC for details.

   procedure Locate_File
     (N     : File_Name_Type;
      T     : File_Type;
      Dir   : Natural;
      Name  : String;
      Found : out File_Name_Type;
      Attr  : access File_Attributes);
   --  See if the file N whose name is Name exists in directory Dir. Dir is an
   --  index into the Lib_Search_Directories table if T = Library. Otherwise
   --  if T = Source, Dir is an index into the Src_Search_Directories table.
   --  Returns the File_Name_Type of the full file name if file found, or
   --  No_File if not found.
   --
   --  On exit, Found is set to the file that was found, and Attr to a cache of
   --  its attributes (at least those that have been computed so far). Reusing
   --  the cache will save some system calls.
   --
   --  Attr is always reset in this call to Unknown_Attributes, even in case of
   --  failure

   procedure Find_File
     (N         : File_Name_Type;
      T         : File_Type;
      Found     : out File_Name_Type;
      Attr      : access File_Attributes;
      Full_Name : Boolean := False);
   --  A version of Find_File that also returns a cache of the file attributes
   --  for later reuse

   procedure Smart_Find_File
     (N     : File_Name_Type;
      T     : File_Type;
      Found : out File_Name_Type;
      Attr  : out File_Attributes);
   --  A version of Smart_Find_File that also returns a cache of the file
   --  attributes for later reuse

   function C_String_Length (S : Address) return CRTL.size_t;
   --  Returns length of a C string (zero for a null address)

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : CRTL.size_t) return String_Access;
   --  Converts a C String to an Ada String. We don't use a more general
   --  purpose facility, because we are dealing with low-level types like
   --  Address. Caller must free result.

   function Include_Dir_Default_Prefix return String_Access;
   --  Same as exported version, except returns a String_Access

   ------------------------------
   -- Other Local Declarations --
   ------------------------------

   EOL : constant Character := ASCII.LF;
   --  End of line character

   Number_File_Names : Nat := 0;
   --  Number of file names found on command line and placed in File_Names

   Look_In_Primary_Directory_For_Current_Main : Boolean := False;
   --  When this variable is True, Find_File only looks in Primary_Directory
   --  for the Current_Main file. This variable is always set to True for the
   --  compiler. It is also True for gnatmake, when the source name given on
   --  the command line has directory information.

   Current_Full_Source_Name  : File_Name_Type  := No_File;
   Current_Full_Source_Stamp : Time_Stamp_Type := Empty_Time_Stamp;
   Current_Full_Lib_Name     : File_Name_Type  := No_File;
   Current_Full_Lib_Stamp    : Time_Stamp_Type := Empty_Time_Stamp;
   Current_Full_Obj_Name     : File_Name_Type  := No_File;
   Current_Full_Obj_Stamp    : Time_Stamp_Type := Empty_Time_Stamp;
   --  Respectively full name (with directory info) and time stamp of the
   --  latest source, library and object files opened by Read_Source_File and
   --  Read_Library_Info.

   package File_Name_Chars is new Table.Table (
     Table_Component_Type => Character,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.File_Name_Chars_Initial,
     Table_Increment      => Alloc.File_Name_Chars_Increment,
     Table_Name           => "File_Name_Chars");
   --  Table to store text to be printed by Dump_Source_File_Names

   The_Include_Dir_Default_Prefix : String_Access := null;
   --  Value returned by Include_Dir_Default_Prefix. We don't initialize it
   --  here, because that causes an elaboration cycle with Sdefault; we
   --  initialize it lazily instead.

   ------------------
   -- Search Paths --
   ------------------

   Primary_Directory : constant := 0;
   --  This is index in the tables created below for the first directory to
   --  search in for source or library information files. This is the directory
   --  containing the latest main input file (a source file for the compiler or
   --  a library file for the binder).

   package Src_Search_Directories is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => Primary_Directory,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Osint.Src_Search_Directories");
   --  Table of names of directories in which to search for source (Compiler)
   --  files. This table is filled in the order in which the directories are
   --  to be searched, and then used in that order.

   package Lib_Search_Directories is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => Primary_Directory,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Osint.Lib_Search_Directories");
   --  Table of names of directories in which to search for library (Binder)
   --  files. This table is filled in the order in which the directories are
   --  to be searched and then used in that order. The reason for having two
   --  distinct tables is that we need them both in gnatmake.

   ---------------------
   -- File Hash Table --
   ---------------------

   --  The file hash table is provided to free the programmer from any
   --  efficiency concern when retrieving full file names or time stamps of
   --  source files. If the programmer calls Source_File_Data (Cache => True)
   --  he is guaranteed that the price to retrieve the full name (i.e. with
   --  directory info) or time stamp of the file will be payed only once, the
   --  first time the full name is actually searched (or the first time the
   --  time stamp is actually retrieved). This is achieved by employing a hash
   --  table that stores as a key the File_Name_Type of the file and associates
   --  to that File_Name_Type the full file name and time stamp of the file.

   File_Cache_Enabled : Boolean := False;
   --  Set to true if you want the enable the file data caching mechanism

   type File_Hash_Num is range 0 .. 1020;

   function File_Hash (F : File_Name_Type) return File_Hash_Num;
   --  Compute hash index for use by Simple_HTable

   type File_Info_Cache is record
      File : File_Name_Type;
      Attr : aliased File_Attributes;
   end record;

   No_File_Info_Cache : constant File_Info_Cache := (No_File, (others => 0));

   package File_Name_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => File_Hash_Num,
     Element    => File_Info_Cache,
     No_Element => No_File_Info_Cache,
     Key        => File_Name_Type,
     Hash       => File_Hash,
     Equal      => "=");

   function Smart_Find_File
     (N : File_Name_Type;
      T : File_Type) return File_Name_Type;
   --  Exactly like Find_File except that if File_Cache_Enabled is True this
   --  routine looks first in the hash table to see if the full name of the
   --  file is already available.

   function Smart_File_Stamp
     (N : File_Name_Type;
      T : File_Type) return Time_Stamp_Type;
   --  Takes the same parameter as the routine above (N is a file name without
   --  any prefix directory information) and behaves like File_Stamp except
   --  that if File_Cache_Enabled is True this routine looks first in the hash
   --  table to see if the file stamp of the file is already available.

   -----------------------------
   -- Add_Default_Search_Dirs --
   -----------------------------

   procedure Add_Default_Search_Dirs is
      Search_Dir     : String_Access;
      Search_Path    : String_Access;
      Path_File_Name : String_Access;

      procedure Add_Search_Dir
        (Search_Dir            : String;
         Additional_Source_Dir : Boolean);
      procedure Add_Search_Dir
        (Search_Dir            : String_Access;
         Additional_Source_Dir : Boolean);
      --  Add a source search dir or a library search dir, depending on the
      --  value of Additional_Source_Dir.

      procedure Get_Dirs_From_File (Additional_Source_Dir : Boolean);
      --  Open a path file and read the directory to search, one per line

      function Get_Libraries_From_Registry return String_Ptr;
      --  On Windows systems, get the list of installed standard libraries
      --  from the registry key:
      --
      --  HKEY_LOCAL_MACHINE\SOFTWARE\Ada Core Technologies\
      --                             GNAT\Standard Libraries
      --  Return an empty string on other systems.
      --
      --  Note that this is an undocumented legacy feature, and that it
      --  works only when using the default runtime library (i.e. no --RTS=
      --  command line switch).

      --------------------
      -- Add_Search_Dir --
      --------------------

      procedure Add_Search_Dir
        (Search_Dir            : String;
         Additional_Source_Dir : Boolean)
      is
      begin
         if Additional_Source_Dir then
            Add_Src_Search_Dir (Search_Dir);
         else
            Add_Lib_Search_Dir (Search_Dir);
         end if;
      end Add_Search_Dir;

      procedure Add_Search_Dir
        (Search_Dir            : String_Access;
         Additional_Source_Dir : Boolean)
      is
      begin
         if Additional_Source_Dir then
            Add_Src_Search_Dir (Search_Dir.all);
         else
            Add_Lib_Search_Dir (Search_Dir.all);
         end if;
      end Add_Search_Dir;

      ------------------------
      -- Get_Dirs_From_File --
      ------------------------

      procedure Get_Dirs_From_File (Additional_Source_Dir : Boolean) is
         File_FD    : File_Descriptor;
         Buffer     : constant String := Path_File_Name.all & ASCII.NUL;
         Len        : Natural;
         Actual_Len : Natural;
         S          : String_Access;
         Curr       : Natural;
         First      : Natural;
         Ch         : Character;

         Status : Boolean;
         pragma Warnings (Off, Status);
         --  For the call to Close where status is ignored

      begin
         File_FD := Open_Read (Buffer'Address, Binary);

         --  If we cannot open the file, we ignore it, we don't fail

         if File_FD = Invalid_FD then
            return;
         end if;

         Len := Integer (File_Length (File_FD));

         S := new String (1 .. Len);

         --  Read the file. Note that the loop is probably not necessary any
         --  more since the whole file is read in at once on all targets. But
         --  it is harmless and might be needed in future.

         Curr := 1;
         Actual_Len := Len;
         while Curr <= Len and then Actual_Len /= 0 loop
            Actual_Len := Read (File_FD, S (Curr)'Address, Len);
            Curr := Curr + Actual_Len;
         end loop;

         --  We are done with the file, so we close it (ignore any error on
         --  the close, since we have successfully read the file).

         Close (File_FD, Status);

         --  Now, we read line by line

         First := 1;
         Curr := 0;
         while Curr < Len loop
            Ch := S (Curr + 1);

            if Ch = ASCII.CR or else Ch = ASCII.LF
              or else Ch = ASCII.FF or else Ch = ASCII.VT
            then
               if First <= Curr then
                  Add_Search_Dir (S (First .. Curr), Additional_Source_Dir);
               end if;

               First := Curr + 2;
            end if;

            Curr := Curr + 1;
         end loop;

         --  Last line is a special case, if the file does not end with
         --  an end of line mark.

         if First <= S'Last then
            Add_Search_Dir (S (First .. S'Last), Additional_Source_Dir);
         end if;
      end Get_Dirs_From_File;

      ---------------------------------
      -- Get_Libraries_From_Registry --
      ---------------------------------

      function Get_Libraries_From_Registry return String_Ptr is
         function C_Get_Libraries_From_Registry return Address;
         pragma Import (C, C_Get_Libraries_From_Registry,
                        "__gnat_get_libraries_from_registry");

         Result_Ptr    : Address;
         Result_Length : CRTL.size_t;
         Out_String    : String_Ptr;

      begin
         Result_Ptr := C_Get_Libraries_From_Registry;
         Result_Length := CRTL.strlen (Result_Ptr);

         Out_String := new String (1 .. Integer (Result_Length));
         CRTL.strncpy (Out_String.all'Address, Result_Ptr, Result_Length);

         CRTL.free (Result_Ptr);

         return Out_String;
      end Get_Libraries_From_Registry;

   --  Start of processing for Add_Default_Search_Dirs

   begin
      --  If there was a -gnateO switch, add all object directories from the
      --  file given in argument to the library search list.

      if Object_Path_File_Name /= null then
         Path_File_Name := String_Access (Object_Path_File_Name);
         pragma Assert (Path_File_Name'Length > 0);
         Get_Dirs_From_File (Additional_Source_Dir => False);
      end if;

      --  After the locations specified on the command line, the next places
      --  to look for files are the directories specified by the appropriate
      --  environment variable. Get this value, extract the directory names
      --  and store in the tables.

      --  Check for eventual project path file env vars

      Path_File_Name := Getenv (Project_Include_Path_File);

      if Path_File_Name'Length > 0 then
         Get_Dirs_From_File (Additional_Source_Dir => True);
      end if;

      Path_File_Name := Getenv (Project_Objects_Path_File);

      if Path_File_Name'Length > 0 then
         Get_Dirs_From_File (Additional_Source_Dir => False);
      end if;

      --  Put path name in canonical form

      for Additional_Source_Dir in False .. True loop
         if Additional_Source_Dir then
            Search_Path := Getenv (Ada_Include_Path);

         else
            Search_Path := Getenv (Ada_Objects_Path);

         end if;

         Get_Next_Dir_In_Path_Init (Search_Path);
         loop
            Search_Dir := Get_Next_Dir_In_Path (Search_Path);
            exit when Search_Dir = null;
            Add_Search_Dir (Search_Dir, Additional_Source_Dir);
         end loop;
      end loop;

      --  For the compiler, if --RTS= was specified, add the runtime
      --  directories.

      if RTS_Src_Path_Name /= null and then RTS_Lib_Path_Name /= null then
         Add_Search_Dirs (RTS_Src_Path_Name, Include);
         Add_Search_Dirs (RTS_Lib_Path_Name, Objects);

      else
         if not Opt.No_Stdinc then

            --  For WIN32 systems, look for any system libraries defined in
            --  the registry. These are added to both source and object
            --  directories.

            Search_Path := String_Access (Get_Libraries_From_Registry);

            Get_Next_Dir_In_Path_Init (Search_Path);
            loop
               Search_Dir := Get_Next_Dir_In_Path (Search_Path);
               exit when Search_Dir = null;
               Add_Search_Dir (Search_Dir, False);
               Add_Search_Dir (Search_Dir, True);
            end loop;

            --  The last place to look are the defaults

            Search_Path :=
              Read_Default_Search_Dirs
                (String_Access (Update_Path (Search_Dir_Prefix)),
                 Include_Search_File,
                 String_Access (Update_Path (Include_Dir_Default_Name)));

            Get_Next_Dir_In_Path_Init (Search_Path);
            loop
               Search_Dir := Get_Next_Dir_In_Path (Search_Path);
               exit when Search_Dir = null;
               Add_Search_Dir (Search_Dir, True);
            end loop;
         end if;

         --  Even when -nostdlib is used, we still want to have visibility on
         --  the run-time object directory, as it is used by gnatbind to find
         --  the run-time ALI files in "real" ZFP set up.

         if not Opt.RTS_Switch then
            Search_Path :=
              Read_Default_Search_Dirs
                (String_Access (Update_Path (Search_Dir_Prefix)),
                 Objects_Search_File,
                 String_Access (Update_Path (Object_Dir_Default_Name)));

            Get_Next_Dir_In_Path_Init (Search_Path);
            loop
               Search_Dir := Get_Next_Dir_In_Path (Search_Path);
               exit when Search_Dir = null;
               Add_Search_Dir (Search_Dir, False);
            end loop;
         end if;
      end if;
   end Add_Default_Search_Dirs;

   --------------
   -- Add_File --
   --------------

   procedure Add_File (File_Name : String; Index : Int := No_Index) is
   begin
      Number_File_Names := Number_File_Names + 1;

      --  As Add_File may be called for mains specified inside a project file,
      --  File_Names may be too short and needs to be extended.

      if Number_File_Names > File_Names'Last then
         File_Names := new File_Name_Array'(File_Names.all & File_Names.all);
         File_Indexes :=
           new File_Index_Array'(File_Indexes.all & File_Indexes.all);
      end if;

      File_Names   (Number_File_Names) := new String'(File_Name);
      File_Indexes (Number_File_Names) := Index;
   end Add_File;

   ------------------------
   -- Add_Lib_Search_Dir --
   ------------------------

   procedure Add_Lib_Search_Dir (Dir : String) is
   begin
      if Dir'Length = 0 then
         Fail ("missing library directory name");
      end if;

      declare
         Norm : String_Ptr := Normalize_Directory_Name (Dir);

      begin
         --  Do nothing if the directory is already in the list. This saves
         --  system calls and avoid unneeded work

         for D in Lib_Search_Directories.First ..
                  Lib_Search_Directories.Last
         loop
            if Lib_Search_Directories.Table (D).all = Norm.all then
               Free (Norm);
               return;
            end if;
         end loop;

         Lib_Search_Directories.Increment_Last;
         Lib_Search_Directories.Table (Lib_Search_Directories.Last) := Norm;
      end;
   end Add_Lib_Search_Dir;

   ---------------------
   -- Add_Search_Dirs --
   ---------------------

   procedure Add_Search_Dirs
     (Search_Path : String_Ptr;
      Path_Type   : Search_File_Type)
   is
      Current_Search_Path : String_Access;

   begin
      Get_Next_Dir_In_Path_Init (String_Access (Search_Path));
      loop
         Current_Search_Path :=
           Get_Next_Dir_In_Path (String_Access (Search_Path));
         exit when Current_Search_Path = null;

         if Path_Type = Include then
            Add_Src_Search_Dir (Current_Search_Path.all);
         else
            Add_Lib_Search_Dir (Current_Search_Path.all);
         end if;
      end loop;
   end Add_Search_Dirs;

   ------------------------
   -- Add_Src_Search_Dir --
   ------------------------

   procedure Add_Src_Search_Dir (Dir : String) is
   begin
      if Dir'Length = 0 then
         Fail ("missing source directory name");
      end if;

      Src_Search_Directories.Increment_Last;
      Src_Search_Directories.Table (Src_Search_Directories.Last) :=
        Normalize_Directory_Name (Dir);
   end Add_Src_Search_Dir;

   --------------------------------
   -- Append_Suffix_To_File_Name --
   --------------------------------

   function Append_Suffix_To_File_Name
     (Name   : File_Name_Type;
      Suffix : String) return File_Name_Type
   is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
      Name_Len := Name_Len + Suffix'Length;
      return Name_Find;
   end Append_Suffix_To_File_Name;

   ---------------------
   -- C_String_Length --
   ---------------------

   function C_String_Length (S : Address) return CRTL.size_t is
   begin
      if S = Null_Address then
         return 0;
      else
         return CRTL.strlen (S);
      end if;
   end C_String_Length;

   ------------------------------
   -- Canonical_Case_File_Name --
   ------------------------------

   procedure Canonical_Case_File_Name (S : in out String) is
   begin
      if not File_Names_Case_Sensitive then
         To_Lower (S);
      end if;
   end Canonical_Case_File_Name;

   ---------------------------------
   -- Canonical_Case_Env_Var_Name --
   ---------------------------------

   procedure Canonical_Case_Env_Var_Name (S : in out String) is
   begin
      if not Env_Vars_Case_Sensitive then
         To_Lower (S);
      end if;
   end Canonical_Case_Env_Var_Name;

   ---------------------------
   -- Create_File_And_Check --
   ---------------------------

   procedure Create_File_And_Check
     (Fdesc : out File_Descriptor;
      Fmode : Mode)
   is
   begin
      Output_File_Name := Name_Enter;
      Fdesc := Create_File (Name_Buffer'Address, Fmode);

      if Fdesc = Invalid_FD then
         Fail ("Cannot create: " & Name_Buffer (1 .. Name_Len));
      end if;
   end Create_File_And_Check;

   -----------------------------------
   -- Open_File_To_Append_And_Check --
   -----------------------------------

   procedure Open_File_To_Append_And_Check
     (Fdesc : out File_Descriptor;
      Fmode : Mode)
   is
   begin
      Output_File_Name := Name_Enter;
      Fdesc := Open_Append (Name_Buffer'Address, Fmode);

      if Fdesc = Invalid_FD then
         Fail ("Cannot create: " & Name_Buffer (1 .. Name_Len));
      end if;
   end Open_File_To_Append_And_Check;

   ------------------------
   -- Current_File_Index --
   ------------------------

   function Current_File_Index return Int is
   begin
      return File_Indexes (Current_File_Name_Index);
   end Current_File_Index;

   --------------------------------
   -- Current_Library_File_Stamp --
   --------------------------------

   function Current_Library_File_Stamp return Time_Stamp_Type is
   begin
      return Current_Full_Lib_Stamp;
   end Current_Library_File_Stamp;

   -------------------------------
   -- Current_Object_File_Stamp --
   -------------------------------

   function Current_Object_File_Stamp return Time_Stamp_Type is
   begin
      return Current_Full_Obj_Stamp;
   end Current_Object_File_Stamp;

   -------------------------------
   -- Current_Source_File_Stamp --
   -------------------------------

   function Current_Source_File_Stamp return Time_Stamp_Type is
   begin
      return Current_Full_Source_Stamp;
   end Current_Source_File_Stamp;

   ----------------------------
   -- Dir_In_Obj_Search_Path --
   ----------------------------

   function Dir_In_Obj_Search_Path (Position : Natural) return String_Ptr is
   begin
      if Opt.Look_In_Primary_Dir then
         return
           Lib_Search_Directories.Table (Primary_Directory + Position - 1);
      else
         return Lib_Search_Directories.Table (Primary_Directory + Position);
      end if;
   end Dir_In_Obj_Search_Path;

   ----------------------------
   -- Dir_In_Src_Search_Path --
   ----------------------------

   function Dir_In_Src_Search_Path (Position : Natural) return String_Ptr is
   begin
      if Opt.Look_In_Primary_Dir then
         return
           Src_Search_Directories.Table (Primary_Directory + Position - 1);
      else
         return Src_Search_Directories.Table (Primary_Directory + Position);
      end if;
   end Dir_In_Src_Search_Path;

   -----------------------------------------
   -- Dump_Command_Line_Source_File_Names --
   -----------------------------------------

   procedure Dump_Command_Line_Source_File_Names is
   begin
      for J in 1 .. Number_Of_Files loop
         Write_Str (File_Names (J).all & " ");
      end loop;
   end Dump_Command_Line_Source_File_Names;

   ----------------------------
   -- Dump_Source_File_Names --
   ----------------------------

   procedure Dump_Source_File_Names is
      subtype Rng is Int range File_Name_Chars.First .. File_Name_Chars.Last;
   begin
      Write_Str (String (File_Name_Chars.Table (Rng)));
   end Dump_Source_File_Names;

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name
     (Name              : File_Name_Type;
      Only_If_No_Suffix : Boolean := False) return File_Name_Type
   is
      Exec_Suffix : String_Access;
      Add_Suffix  : Boolean;

   begin
      if Name = No_File then
         return No_File;
      end if;

      if Executable_Extension_On_Target = No_Name then
         Exec_Suffix := Get_Target_Executable_Suffix;
      else
         Get_Name_String (Executable_Extension_On_Target);
         Exec_Suffix := new String'(Name_Buffer (1 .. Name_Len));
      end if;

      if Exec_Suffix'Length /= 0 then
         Get_Name_String (Name);

         Add_Suffix := True;
         if Only_If_No_Suffix then
            for J in reverse 1 .. Name_Len loop
               if Name_Buffer (J) = '.' then
                  Add_Suffix := False;
                  exit;

               elsif Is_Directory_Separator (Name_Buffer (J)) then
                  exit;
               end if;
            end loop;
         end if;

         if Add_Suffix then
            --  If Executable doesn't end with the executable suffix, add it

            if Name_Len <= Exec_Suffix'Length
              or else not
                File_Names_Equal
                  (Name_Buffer
                    (Name_Len - Exec_Suffix'Length + 1 .. Name_Len),
                   Exec_Suffix.all)
            then
               Name_Buffer
                 (Name_Len + 1 .. Name_Len + Exec_Suffix'Length) :=
                   Exec_Suffix.all;
               Name_Len := Name_Len + Exec_Suffix'Length;
               Free (Exec_Suffix);
               return Name_Find;
            end if;
         end if;
      end if;

      Free (Exec_Suffix);
      return Name;
   end Executable_Name;

   function Executable_Name
     (Name              : String;
      Only_If_No_Suffix : Boolean := False) return String
   is
      Exec_Suffix    : String_Access;
      Add_Suffix     : Boolean;

   begin
      if Executable_Extension_On_Target = No_Name then
         Exec_Suffix := Get_Target_Executable_Suffix;
      else
         Get_Name_String (Executable_Extension_On_Target);
         Exec_Suffix := new String'(Name_Buffer (1 .. Name_Len));
      end if;

      if Exec_Suffix'Length = 0 then
         Free (Exec_Suffix);
         return Name;

      else
         declare
            Suffix : constant String := Exec_Suffix.all;

         begin
            Free (Exec_Suffix);
            Add_Suffix := True;

            if Only_If_No_Suffix then
               for J in reverse Name'Range loop
                  if Name (J) = '.' then
                     Add_Suffix := False;
                     exit;

                  elsif Is_Directory_Separator (Name (J)) then
                     exit;
                  end if;
               end loop;
            end if;

            if Add_Suffix and then
              (Name'Length <= Suffix'Length
               or else not
                 File_Names_Equal
                   (Name (Name'Last - Suffix'Length + 1 .. Name'Last),
                    Suffix))
            then
               declare
                  Result : String (1 .. Name'Length + Suffix'Length);
               begin
                  Result (1 .. Name'Length) := Name;
                  Result (Name'Length + 1 .. Result'Last) := Suffix;
                  return Result;
               end;
            else
               return Name;
            end if;
         end;
      end if;
   end Executable_Name;

   -----------------------
   -- Executable_Prefix --
   -----------------------

   function Executable_Prefix return String_Ptr is

      function Get_Install_Dir (Exec : String) return String_Ptr;
      --  S is the executable name preceded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (Exec : String) return String_Ptr is
         Full_Path : constant String := Normalize_Pathname (Exec);
         --  Use the full path, so that we find "lib" or "bin", even when
         --  the tool has been invoked with a relative path, as in
         --  "./gnatls -v" invoked in the GNAT bin directory.

      begin
         for J in reverse Full_Path'Range loop
            if Is_Directory_Separator (Full_Path (J)) then
               if J < Full_Path'Last - 5 then
                  if (To_Lower (Full_Path (J + 1)) = 'l'
                      and then To_Lower (Full_Path (J + 2)) = 'i'
                      and then To_Lower (Full_Path (J + 3)) = 'b')
                    or else
                      (To_Lower (Full_Path (J + 1)) = 'b'
                       and then To_Lower (Full_Path (J + 2)) = 'i'
                       and then To_Lower (Full_Path (J + 3)) = 'n')
                  then
                     return new String'(Full_Path (Full_Path'First .. J));
                  end if;
               end if;
            end if;
         end loop;

         return new String'("");
      end Get_Install_Dir;

   --  Start of processing for Executable_Prefix

   begin
      if Exec_Name = null then
         Exec_Name := new String (1 .. Len_Arg (0));
         Osint.Fill_Arg (Exec_Name (1)'Address, 0);
      end if;

      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name.all);
         end if;
      end loop;

      --  If we come here, the user has typed the executable name with no
      --  directory prefix.

      return Get_Install_Dir (Locate_Exec_On_Path (Exec_Name.all).all);
   end Executable_Prefix;

   ------------------
   -- Exit_Program --
   ------------------

   procedure Exit_Program (Exit_Code : Exit_Code_Type) is
   begin
      --  The program will exit with the following status:

      --    0 if the object file has been generated (with or without warnings)
      --    1 if recompilation was not needed (smart recompilation)
      --    2 if gnat1 has been killed by a signal (detected by GCC)
      --    4 for a fatal error
      --    5 if there were errors
      --    6 if no code has been generated (spec)

      --  Note that exit code 3 is not used and must not be used as this is
      --  the code returned by a program aborted via C abort() routine on
      --  Windows. GCC checks for that case and thinks that the child process
      --  has been aborted. This code (exit code 3) used to be the code used
      --  for E_No_Code, but E_No_Code was changed to 6 for this reason.

      case Exit_Code is
         when E_Success    => OS_Exit (0);
         when E_Warnings   => OS_Exit (0);
         when E_No_Compile => OS_Exit (1);
         when E_Fatal      => OS_Exit (4);
         when E_Errors     => OS_Exit (5);
         when E_No_Code    => OS_Exit (6);
         when E_Abort      => OS_Abort;
      end case;
   end Exit_Program;

   ----------
   -- Fail --
   ----------

   procedure Fail (S : String) is
   begin
      --  We use Output in case there is a special output set up. In this case
      --  Set_Standard_Error will have no immediate effect.

      Set_Standard_Error;
      Osint.Write_Program_Name;
      Write_Str (": ");
      Write_Str (S);
      Write_Eol;

      Exit_Program (E_Fatal);
   end Fail;

   ----------------------
   -- File_Names_Equal --
   ----------------------

   function File_Names_Equal (File1, File2 : String) return Boolean is

      function To_Lower (A : String) return String;
      --  For bootstrap reasons, we cannot use To_Lower function from
      --  System.Case_Util.

      --------------
      -- To_Lower --
      --------------

      function To_Lower (A : String) return String is
         Result : String := A;
      begin
         To_Lower (Result);
         return Result;
      end To_Lower;

   --  Start of processing for File_Names_Equal

   begin
      if File_Names_Case_Sensitive then
         return File1 = File2;
      else
         return To_Lower (File1) = To_Lower (File2);
      end if;
   end File_Names_Equal;

   ---------------
   -- File_Hash --
   ---------------

   function File_Hash (F : File_Name_Type) return File_Hash_Num is
   begin
      return File_Hash_Num (Int (F) mod File_Hash_Num'Range_Length);
   end File_Hash;

   -----------------
   -- File_Length --
   -----------------

   function File_Length
     (Name : C_File_Name;
      Attr : access File_Attributes) return Long_Integer
   is
      function Internal
        (F : Integer;
         N : C_File_Name;
         A : System.Address) return CRTL.int64;
      pragma Import (C, Internal, "__gnat_file_length_attr");

   begin
      --  The conversion from int64 to Long_Integer is ok here as this
      --  routine is only to be used by the compiler and we do not expect
      --  a unit to be larger than a 32bit integer.

      return Long_Integer (Internal (-1, Name, Attr.all'Address));
   end File_Length;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Name : C_File_Name;
      Attr : access File_Attributes) return OS_Time
   is
      function Internal (N : C_File_Name; A : System.Address) return OS_Time;
      pragma Import (C, Internal, "__gnat_file_time_name_attr");
   begin
      return Internal (Name, Attr.all'Address);
   end File_Time_Stamp;

   function File_Time_Stamp
     (Name : Path_Name_Type;
      Attr : access File_Attributes) return Time_Stamp_Type
   is
   begin
      if Name = No_Path then
         return Empty_Time_Stamp;
      end if;

      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1) := ASCII.NUL;
      return OS_Time_To_GNAT_Time
               (File_Time_Stamp (Name_Buffer'Address, Attr));
   end File_Time_Stamp;

   ----------------
   -- File_Stamp --
   ----------------

   function File_Stamp (Name : File_Name_Type) return Time_Stamp_Type is
   begin
      if Name = No_File then
         return Empty_Time_Stamp;
      end if;

      Get_Name_String (Name);

      --  File_Time_Stamp will always return Invalid_Time if the file does
      --  not exist, and OS_Time_To_GNAT_Time will convert this value to
      --  Empty_Time_Stamp. Therefore we do not need to first test whether
      --  the file actually exists, which saves a system call.

      return OS_Time_To_GNAT_Time
               (File_Time_Stamp (Name_Buffer (1 .. Name_Len)));
   end File_Stamp;

   function File_Stamp (Name : Path_Name_Type) return Time_Stamp_Type is
   begin
      return File_Stamp (File_Name_Type (Name));
   end File_Stamp;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (N         : File_Name_Type;
      T         : File_Type;
      Full_Name : Boolean := False) return File_Name_Type
   is
      Attr  : aliased File_Attributes;
      Found : File_Name_Type;
   begin
      Find_File (N, T, Found, Attr'Access, Full_Name);
      return Found;
   end Find_File;

   ---------------
   -- Find_File --
   ---------------

   procedure Find_File
     (N         : File_Name_Type;
      T         : File_Type;
      Found     : out File_Name_Type;
      Attr      : access File_Attributes;
      Full_Name : Boolean := False)
   is
   begin
      Get_Name_String (N);

      declare
         File_Name : String renames Name_Buffer (1 .. Name_Len);
         File      : File_Name_Type := No_File;
         Last_Dir  : Natural;

      begin
         --  If we are looking for a config file, look only in the current
         --  directory, i.e. return input argument unchanged. Also look only in
         --  the current directory if we are looking for a .dg file (happens in
         --  -gnatD mode).

         if T = Config
           or else (Debug_Generated_Code
                     and then Name_Len > 3
                     and then Name_Buffer (Name_Len - 2 .. Name_Len) = ".dg")
         then
            Found := N;
            Attr.all := Unknown_Attributes;

            if T = Config then
               if Full_Name then
                  declare
                     Full_Path : constant String :=
                                   Normalize_Pathname (Get_Name_String (N));
                     Full_Size : constant Natural := Full_Path'Length;

                  begin
                     Name_Buffer (1 .. Full_Size) := Full_Path;
                     Name_Len := Full_Size;
                     Found    := Name_Find;
                  end;
               end if;

               --  Check that it is a file, not a directory

               if not Is_Regular_File (Get_Name_String (Found)) then
                  Found := No_File;
               end if;
            end if;

            return;

         --  If we are trying to find the current main file just look in the
         --  directory where the user said it was.

         elsif Look_In_Primary_Directory_For_Current_Main
           and then Current_Main = N
         then
            Locate_File (N, T, Primary_Directory, File_Name, Found, Attr);
            return;

         --  Otherwise do standard search for source file

         else
            --  Check the mapping of this file name

            File := Mapped_Path_Name (N);

            --  If the file name is mapped to a path name, return the
            --  corresponding path name

            if File /= No_File then

               --  For locally removed file, Error_Name is returned; then
               --  return No_File, indicating the file is not a source.

               if File = Error_File_Name then
                  Found := No_File;
               else
                  Found := File;
               end if;

               Attr.all := Unknown_Attributes;
               return;
            end if;

            --  First place to look is in the primary directory (i.e. the same
            --  directory as the source) unless this has been disabled with -I-

            if Opt.Look_In_Primary_Dir then
               Locate_File (N, T, Primary_Directory, File_Name, Found, Attr);

               if Found /= No_File then
                  return;
               end if;
            end if;

            --  Finally look in directories specified with switches -I/-aI/-aO

            if T = Library then
               Last_Dir := Lib_Search_Directories.Last;
            else
               Last_Dir := Src_Search_Directories.Last;
            end if;

            for D in Primary_Directory + 1 .. Last_Dir loop
               Locate_File (N, T, D, File_Name, Found, Attr);

               if Found /= No_File then
                  return;
               end if;
            end loop;

            Attr.all := Unknown_Attributes;
            Found := No_File;
         end if;
      end;
   end Find_File;

   -----------------------
   -- Find_Program_Name --
   -----------------------

   procedure Find_Program_Name is
      Command_Name : String (1 .. Len_Arg (0));
      Cindex1      : Integer := Command_Name'First;
      Cindex2      : Integer := Command_Name'Last;

   begin
      Fill_Arg (Command_Name'Address, 0);

      if Command_Name = "" then
         Name_Len := 0;
         return;
      end if;

      --  The program name might be specified by a full path name. However,
      --  we don't want to print that all out in an error message, so the
      --  path might need to be stripped away.

      for J in reverse Cindex1 .. Cindex2 loop
         if Is_Directory_Separator (Command_Name (J)) then
            Cindex1 := J + 1;
            exit;
         end if;
      end loop;

      --  Command_Name(Cindex1 .. Cindex2) is now the equivalent of the
      --  POSIX command "basename argv[0]"

      --  Strip off any executable extension (usually nothing or .exe)
      --  but formally reported by autoconf in the variable EXEEXT

      if Cindex2 - Cindex1 >= 4 then
         if To_Lower (Command_Name (Cindex2 - 3)) = '.'
            and then To_Lower (Command_Name (Cindex2 - 2)) = 'e'
            and then To_Lower (Command_Name (Cindex2 - 1)) = 'x'
            and then To_Lower (Command_Name (Cindex2)) = 'e'
         then
            Cindex2 := Cindex2 - 4;
         end if;
      end if;

      Name_Len := Cindex2 - Cindex1 + 1;
      Name_Buffer (1 .. Name_Len) := Command_Name (Cindex1 .. Cindex2);
   end Find_Program_Name;

   ------------------------
   -- Full_Lib_File_Name --
   ------------------------

   procedure Full_Lib_File_Name
     (N        : File_Name_Type;
      Lib_File : out File_Name_Type;
      Attr     : out File_Attributes)
   is
   begin
      Smart_Find_File (N, Library, Lib_File, Attr);
   end Full_Lib_File_Name;

   ------------------------
   -- Full_Lib_File_Name --
   ------------------------

   function Full_Lib_File_Name (N : File_Name_Type) return File_Name_Type is
      Attr : File_Attributes;
      File : File_Name_Type;
   begin
      Full_Lib_File_Name (N, File, Attr);
      return File;
   end Full_Lib_File_Name;

   ----------------------------
   -- Full_Library_Info_Name --
   ----------------------------

   function Full_Library_Info_Name return File_Name_Type is
   begin
      return Current_Full_Lib_Name;
   end Full_Library_Info_Name;

   ---------------------------
   -- Full_Object_File_Name --
   ---------------------------

   function Full_Object_File_Name return File_Name_Type is
   begin
      return Current_Full_Obj_Name;
   end Full_Object_File_Name;

   ----------------------
   -- Full_Source_Name --
   ----------------------

   function Full_Source_Name return File_Name_Type is
   begin
      return Current_Full_Source_Name;
   end Full_Source_Name;

   ----------------------
   -- Full_Source_Name --
   ----------------------

   function Full_Source_Name (N : File_Name_Type) return File_Name_Type is
   begin
      return Smart_Find_File (N, Source);
   end Full_Source_Name;

   ----------------------
   -- Full_Source_Name --
   ----------------------

   procedure Full_Source_Name
     (N         : File_Name_Type;
      Full_File : out File_Name_Type;
      Attr      : access File_Attributes) is
   begin
      Smart_Find_File (N, Source, Full_File, Attr.all);
   end Full_Source_Name;

   -------------------
   -- Get_Directory --
   -------------------

   function Get_Directory (Name : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Name);

      for J in reverse 1 .. Name_Len loop
         if Is_Directory_Separator (Name_Buffer (J)) then
            Name_Len := J;
            return Name_Find;
         end if;
      end loop;

      Name_Len := Hostparm.Normalized_CWD'Length;
      Name_Buffer (1 .. Name_Len) := Hostparm.Normalized_CWD;
      return Name_Find;
   end Get_Directory;

   ------------------------------
   -- Get_First_Main_File_Name --
   ------------------------------

   function Get_First_Main_File_Name return String is
   begin
      return File_Names (1).all;
   end Get_First_Main_File_Name;

   --------------------------
   -- Get_Next_Dir_In_Path --
   --------------------------

   Search_Path_Pos : Integer;
   --  Keeps track of current position in search path. Initialized by the
   --  call to Get_Next_Dir_In_Path_Init, updated by Get_Next_Dir_In_Path.

   function Get_Next_Dir_In_Path
     (Search_Path : String_Access) return String_Access
   is
      Lower_Bound : Positive := Search_Path_Pos;
      Upper_Bound : Positive;

   begin
      loop
         while Lower_Bound <= Search_Path'Last
           and then Search_Path.all (Lower_Bound) = Path_Separator
         loop
            Lower_Bound := Lower_Bound + 1;
         end loop;

         exit when Lower_Bound > Search_Path'Last;

         Upper_Bound := Lower_Bound;
         while Upper_Bound <= Search_Path'Last
           and then Search_Path.all (Upper_Bound) /= Path_Separator
         loop
            Upper_Bound := Upper_Bound + 1;
         end loop;

         Search_Path_Pos := Upper_Bound;
         return new String'(Search_Path.all (Lower_Bound .. Upper_Bound - 1));
      end loop;

      return null;
   end Get_Next_Dir_In_Path;

   -------------------------------
   -- Get_Next_Dir_In_Path_Init --
   -------------------------------

   procedure Get_Next_Dir_In_Path_Init (Search_Path : String_Access) is
   begin
      Search_Path_Pos := Search_Path'First;
   end Get_Next_Dir_In_Path_Init;

   --------------------------------------
   -- Get_Primary_Src_Search_Directory --
   --------------------------------------

   function Get_Primary_Src_Search_Directory return String_Ptr is
   begin
      return Src_Search_Directories.Table (Primary_Directory);
   end Get_Primary_Src_Search_Directory;

   ------------------------
   -- Get_RTS_Search_Dir --
   ------------------------

   function Get_RTS_Search_Dir
     (Search_Dir : String;
      File_Type  : Search_File_Type) return String_Ptr
   is
      procedure Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Get_Current_Dir, "__gnat_get_current_dir");

      Max_Path : Integer;
      pragma Import (C, Max_Path, "__gnat_max_path_len");
      --  Maximum length of a path name

      Current_Dir        : String_Ptr;
      Default_Search_Dir : String_Access;
      Default_Suffix_Dir : String_Access;
      Local_Search_Dir   : String_Access;
      Norm_Search_Dir    : String_Access;
      Result_Search_Dir  : String_Access;
      Search_File        : String_Access;
      Temp_String        : String_Ptr;

   begin
      --  Add a directory separator at the end of the directory if necessary
      --  so that we can directly append a file to the directory

      if not Is_Directory_Separator (Search_Dir (Search_Dir'Last)) then
         Local_Search_Dir :=
           new String'(Search_Dir & String'(1 => Directory_Separator));
      else
         Local_Search_Dir := new String'(Search_Dir);
      end if;

      if File_Type = Include then
         Search_File := Include_Search_File;
         Default_Suffix_Dir := new String'("adainclude");
      else
         Search_File := Objects_Search_File;
         Default_Suffix_Dir := new String'("adalib");
      end if;

      Norm_Search_Dir := Local_Search_Dir;

      if Is_Absolute_Path (Norm_Search_Dir.all) then

         --  We first verify if there is a directory Include_Search_Dir
         --  containing default search directories

         Result_Search_Dir :=
           Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);
         Default_Search_Dir :=
           new String'(Norm_Search_Dir.all & Default_Suffix_Dir.all);
         Free (Norm_Search_Dir);

         if Result_Search_Dir /= null then
            return String_Ptr (Result_Search_Dir);
         elsif Is_Directory (Default_Search_Dir.all) then
            return String_Ptr (Default_Search_Dir);
         else
            return null;
         end if;

      --  Search in the current directory

      else
         --  Get the current directory

         declare
            Buffer   : String (1 .. Max_Path + 2);
            Path_Len : Natural := Max_Path;

         begin
            Get_Current_Dir (Buffer'Address, Path_Len'Address);

            if Path_Len = 0 then
               raise Program_Error;
            end if;

            if not Is_Directory_Separator (Buffer (Path_Len)) then
               Path_Len := Path_Len + 1;
               Buffer (Path_Len) := Directory_Separator;
            end if;

            Current_Dir := new String'(Buffer (1 .. Path_Len));
         end;

         Norm_Search_Dir :=
           new String'(Current_Dir.all & Local_Search_Dir.all);

         Result_Search_Dir :=
           Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);

         Default_Search_Dir :=
           new String'(Norm_Search_Dir.all & Default_Suffix_Dir.all);

         Free (Norm_Search_Dir);

         if Result_Search_Dir /= null then
            return String_Ptr (Result_Search_Dir);

         elsif Is_Directory (Default_Search_Dir.all) then
            return String_Ptr (Default_Search_Dir);

         else
            --  Search in Search_Dir_Prefix/Search_Dir

            Norm_Search_Dir :=
              new String'
               (Update_Path (Search_Dir_Prefix).all & Local_Search_Dir.all);

            Result_Search_Dir :=
              Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);

            Default_Search_Dir :=
              new String'(Norm_Search_Dir.all & Default_Suffix_Dir.all);

            Free (Norm_Search_Dir);

            if Result_Search_Dir /= null then
               return String_Ptr (Result_Search_Dir);

            elsif Is_Directory (Default_Search_Dir.all) then
               return String_Ptr (Default_Search_Dir);

            else
               --  We finally search in Search_Dir_Prefix/rts-Search_Dir

               Temp_String :=
                 new String'(Update_Path (Search_Dir_Prefix).all & "rts-");

               Norm_Search_Dir :=
                 new String'(Temp_String.all & Local_Search_Dir.all);

               Result_Search_Dir :=
                 Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);

               Default_Search_Dir :=
                 new String'(Norm_Search_Dir.all & Default_Suffix_Dir.all);
               Free (Norm_Search_Dir);

               if Result_Search_Dir /= null then
                  return String_Ptr (Result_Search_Dir);

               elsif Is_Directory (Default_Search_Dir.all) then
                  return String_Ptr (Default_Search_Dir);

               else
                  return null;
               end if;
            end if;
         end if;
      end if;
   end Get_RTS_Search_Dir;

   --------------------------------
   -- Include_Dir_Default_Prefix --
   --------------------------------

   function Include_Dir_Default_Prefix return String_Access is
   begin
      if The_Include_Dir_Default_Prefix = null then
         The_Include_Dir_Default_Prefix :=
           String_Access (Update_Path (Include_Dir_Default_Name));
      end if;

      return The_Include_Dir_Default_Prefix;
   end Include_Dir_Default_Prefix;

   function Include_Dir_Default_Prefix return String is
   begin
      return Include_Dir_Default_Prefix.all;
   end Include_Dir_Default_Prefix;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Number_File_Names       := 0;
      Current_File_Name_Index := 0;

      Src_Search_Directories.Init;
      Lib_Search_Directories.Init;

      --  Start off by setting all suppress options, to False. The special
      --  overflow fields are set to Not_Set (they will be set by -gnatp, or
      --  by -gnato, or, if neither of these appear, in Adjust_Global_Switches
      --  in Gnat1drv).

      Suppress_Options := ((others => False), Not_Set, Not_Set);

      --  Reserve the first slot in the search paths table. This is the
      --  directory of the main source file or main library file and is filled
      --  in by each call to Next_Main_Source/Next_Main_Lib_File with the
      --  directory specified for this main source or library file. This is the
      --  directory which is searched first by default. This default search is
      --  inhibited by the option -I- for both source and library files.

      Src_Search_Directories.Set_Last (Primary_Directory);
      Src_Search_Directories.Table (Primary_Directory) := new String'("");

      Lib_Search_Directories.Set_Last (Primary_Directory);
      Lib_Search_Directories.Table (Primary_Directory) := new String'("");
   end Initialize;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Name : C_File_Name; Attr : access File_Attributes) return Boolean
   is
      function Internal (N : C_File_Name; A : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_is_directory_attr");
   begin
      return Internal (Name, Attr.all'Address) /= 0;
   end Is_Directory;

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      --  In addition to the default directory_separator allow the '/' to
      --  act as separator since this is allowed in MS-DOS and Windows.

      return C = Directory_Separator or else C = '/';
   end Is_Directory_Separator;

   -------------------------
   -- Is_Readonly_Library --
   -------------------------

   function Is_Readonly_Library (File : File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);

      pragma Assert (Name_Buffer (Name_Len - 3 .. Name_Len) = ".ali");

      return not Is_Writable_File (Name_Buffer (1 .. Name_Len));
   end Is_Readonly_Library;

   ------------------------
   -- Is_Executable_File --
   ------------------------

   function Is_Executable_File
     (Name : C_File_Name; Attr : access File_Attributes) return Boolean
   is
      function Internal (N : C_File_Name; A : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_is_executable_file_attr");
   begin
      return Internal (Name, Attr.all'Address) /= 0;
   end Is_Executable_File;

   ----------------------
   -- Is_Readable_File --
   ----------------------

   function Is_Readable_File
     (Name : C_File_Name; Attr : access File_Attributes) return Boolean
   is
      function Internal (N : C_File_Name; A : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_is_readable_file_attr");
   begin
      return Internal (Name, Attr.all'Address) /= 0;
   end Is_Readable_File;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Name : C_File_Name; Attr : access File_Attributes) return Boolean
   is
      function Internal (N : C_File_Name; A : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_is_regular_file_attr");
   begin
      return Internal (Name, Attr.all'Address) /= 0;
   end Is_Regular_File;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link
     (Name : C_File_Name; Attr : access File_Attributes) return Boolean
   is
      function Internal (N : C_File_Name; A : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_is_symbolic_link_attr");
   begin
      return Internal (Name, Attr.all'Address) /= 0;
   end Is_Symbolic_Link;

   ----------------------
   -- Is_Writable_File --
   ----------------------

   function Is_Writable_File
     (Name : C_File_Name; Attr : access File_Attributes) return Boolean
   is
      function Internal (N : C_File_Name; A : System.Address) return Integer;
      pragma Import (C, Internal, "__gnat_is_writable_file_attr");
   begin
      return Internal (Name, Attr.all'Address) /= 0;
   end Is_Writable_File;

   -------------------
   -- Lib_File_Name --
   -------------------

   function Lib_File_Name
     (Source_File : File_Name_Type;
      Munit_Index : Nat := 0) return File_Name_Type
   is
   begin
      Get_Name_String (Source_File);

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            exit;
         end if;
      end loop;

      if Munit_Index /= 0 then
         Add_Char_To_Name_Buffer (Multi_Unit_Index_Character);
         Add_Nat_To_Name_Buffer (Munit_Index);
      end if;

      Add_Char_To_Name_Buffer ('.');
      Add_Str_To_Name_Buffer (ALI_Suffix.all);
      return Name_Find;
   end Lib_File_Name;

   -----------------
   -- Locate_File --
   -----------------

   procedure Locate_File
     (N     : File_Name_Type;
      T     : File_Type;
      Dir   : Natural;
      Name  : String;
      Found : out File_Name_Type;
      Attr  : access File_Attributes)
   is
      Dir_Name : String_Ptr;

   begin
      --  If Name is already an absolute path, do not look for a directory

      if Is_Absolute_Path (Name) then
         Dir_Name := No_Dir;

      elsif T = Library then
         Dir_Name := Lib_Search_Directories.Table (Dir);

      else
         pragma Assert (T /= Config);
         Dir_Name := Src_Search_Directories.Table (Dir);
      end if;

      declare
         Full_Name :
           constant String (1 .. Dir_Name'Length + Name'Length + 1) :=
           Dir_Name.all & Name & ASCII.NUL;
         --  Use explicit bounds, because Dir_Name might be a substring whose
         --  'First is not 1.

      begin
         Attr.all := Unknown_Attributes;

         if not Is_Regular_File (Full_Name'Address, Attr) then
            Found := No_File;

         else
            --  If the file is in the current directory then return N itself

            if Dir_Name'Length = 0 then
               Found := N;
            else
               Found :=
                 Name_Find (Full_Name (Full_Name'First .. Full_Name'Last - 1));
            end if;
         end if;
      end;
   end Locate_File;

   -------------------------------
   -- Matching_Full_Source_Name --
   -------------------------------

   function Matching_Full_Source_Name
     (N : File_Name_Type;
      T : Time_Stamp_Type) return File_Name_Type
   is
   begin
      Get_Name_String (N);

      declare
         File_Name : constant String := Name_Buffer (1 .. Name_Len);
         File      : File_Name_Type := No_File;
         Attr      : aliased File_Attributes;
         Last_Dir  : Natural;

      begin
         if Opt.Look_In_Primary_Dir then
            Locate_File
              (N, Source, Primary_Directory, File_Name, File,
               Attr'Unchecked_Access);

            if File /= No_File and then T = File_Stamp (N) then
               return File;
            end if;
         end if;

         Last_Dir := Src_Search_Directories.Last;

         for D in Primary_Directory + 1 .. Last_Dir loop
            Locate_File (N, Source, D, File_Name, File, Attr'Unchecked_Access);

            if File /= No_File and then T = File_Stamp (File) then
               return File;
            end if;
         end loop;

         return No_File;
      end;
   end Matching_Full_Source_Name;

   ----------------
   -- More_Files --
   ----------------

   function More_Files return Boolean is
   begin
      return (Current_File_Name_Index < Number_File_Names);
   end More_Files;

   -------------------------------
   -- Nb_Dir_In_Obj_Search_Path --
   -------------------------------

   function Nb_Dir_In_Obj_Search_Path return Natural is
   begin
      if Opt.Look_In_Primary_Dir then
         return Lib_Search_Directories.Last - Primary_Directory + 1;
      else
         return Lib_Search_Directories.Last - Primary_Directory;
      end if;
   end Nb_Dir_In_Obj_Search_Path;

   -------------------------------
   -- Nb_Dir_In_Src_Search_Path --
   -------------------------------

   function Nb_Dir_In_Src_Search_Path return Natural is
   begin
      if Opt.Look_In_Primary_Dir then
         return Src_Search_Directories.Last - Primary_Directory + 1;
      else
         return Src_Search_Directories.Last - Primary_Directory;
      end if;
   end Nb_Dir_In_Src_Search_Path;

   --------------------
   -- Next_Main_File --
   --------------------

   function Next_Main_File return File_Name_Type is
      File_Name : String_Ptr;
      Dir_Name  : String_Ptr;
      Fptr      : Natural;

   begin
      pragma Assert (More_Files);

      Current_File_Name_Index := Current_File_Name_Index + 1;

      --  Get the file and directory name

      File_Name := File_Names (Current_File_Name_Index);
      Fptr := File_Name'First;

      for J in reverse File_Name'Range loop
         if Is_Directory_Separator (File_Name (J)) then
            if J = File_Name'Last then
               Fail ("File name missing");
            end if;

            Fptr := J + 1;
            exit;
         end if;
      end loop;

      --  Save name of directory in which main unit resides for use in
      --  locating other units

      Dir_Name := new String'(File_Name (File_Name'First .. Fptr - 1));

      case Running_Program is
         when Compiler =>
            Src_Search_Directories.Table (Primary_Directory) := Dir_Name;
            Look_In_Primary_Directory_For_Current_Main := True;

         when Make =>
            Src_Search_Directories.Table (Primary_Directory) := Dir_Name;

            if Fptr > File_Name'First then
               Look_In_Primary_Directory_For_Current_Main := True;
            end if;

         when Binder
            | Gnatls
         =>
            Dir_Name := Normalize_Directory_Name (Dir_Name.all);
            Lib_Search_Directories.Table (Primary_Directory) := Dir_Name;

         when Unspecified =>
            null;
      end case;

      Name_Len := File_Name'Last - Fptr + 1;
      Name_Buffer (1 .. Name_Len) := File_Name (Fptr .. File_Name'Last);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Current_Main := Name_Find;

      --  In the gnatmake case, the main file may have not have the
      --  extension. Try ".adb" first then ".ads"

      if Running_Program = Make then
         declare
            Orig_Main : constant File_Name_Type := Current_Main;

         begin
            if Strip_Suffix (Orig_Main) = Orig_Main then
               Current_Main :=
                 Append_Suffix_To_File_Name (Orig_Main, ".adb");

               if Full_Source_Name (Current_Main) = No_File then
                  Current_Main :=
                    Append_Suffix_To_File_Name (Orig_Main, ".ads");

                  if Full_Source_Name (Current_Main) = No_File then
                     Current_Main := Orig_Main;
                  end if;
               end if;
            end if;
         end;
      end if;

      return Current_Main;
   end Next_Main_File;

   ------------------------------
   -- Normalize_Directory_Name --
   ------------------------------

   function Normalize_Directory_Name (Directory : String) return String_Ptr is

      function Is_Quoted (Path : String) return Boolean;
      pragma Inline (Is_Quoted);
      --  Returns true if Path is quoted (either double or single quotes)

      ---------------
      -- Is_Quoted --
      ---------------

      function Is_Quoted (Path : String) return Boolean is
         First : constant Character := Path (Path'First);
         Last  : constant Character := Path (Path'Last);

      begin
         if (First = ''' and then Last = ''')
               or else
            (First = '"' and then Last = '"')
         then
            return True;
         else
            return False;
         end if;
      end Is_Quoted;

      Result : String_Ptr;

   --  Start of processing for Normalize_Directory_Name

   begin
      if Directory'Length = 0 then
         Result := new String'(Hostparm.Normalized_CWD);

      elsif Is_Directory_Separator (Directory (Directory'Last)) then
         Result := new String'(Directory);

      elsif Is_Quoted (Directory) then

         --  This is a quoted string, it certainly means that the directory
         --  contains some spaces for example. We can safely remove the quotes
         --  here as the OS_Lib.Normalize_Arguments will be called before any
         --  spawn routines. This ensure that quotes will be added when needed.

         Result := new String (1 .. Directory'Length - 1);
         Result (1 .. Directory'Length - 2) :=
           Directory (Directory'First + 1 .. Directory'Last - 1);
         Result (Result'Last) := Directory_Separator;

      else
         Result := new String (1 .. Directory'Length + 1);
         Result (1 .. Directory'Length) := Directory;
         Result (Directory'Length + 1) := Directory_Separator;
      end if;

      return Result;
   end Normalize_Directory_Name;

   ---------------------
   -- Number_Of_Files --
   ---------------------

   function Number_Of_Files return Nat is
   begin
      return Number_File_Names;
   end Number_Of_Files;

   -------------------------------
   -- Object_Dir_Default_Prefix --
   -------------------------------

   function Object_Dir_Default_Prefix return String is
      Object_Dir : String_Access :=
                     String_Access (Update_Path (Object_Dir_Default_Name));

   begin
      if Object_Dir = null then
         return "";

      else
         declare
            Result : constant String := Object_Dir.all;
         begin
            Free (Object_Dir);
            return Result;
         end;
      end if;
   end Object_Dir_Default_Prefix;

   ----------------------
   -- Object_File_Name --
   ----------------------

   function Object_File_Name (N : File_Name_Type) return File_Name_Type is
   begin
      if N = No_File then
         return No_File;
      end if;

      Get_Name_String (N);
      Name_Len := Name_Len - ALI_Suffix'Length - 1;

      for J in Target_Object_Suffix'Range loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Target_Object_Suffix (J);
      end loop;

      return Name_Enter;
   end Object_File_Name;

   -------------------------------
   -- OS_Exit_Through_Exception --
   -------------------------------

   procedure OS_Exit_Through_Exception (Status : Integer) is
   begin
      Current_Exit_Status := Status;
      raise Types.Terminate_Program;
   end OS_Exit_Through_Exception;

   --------------------------
   -- OS_Time_To_GNAT_Time --
   --------------------------

   function OS_Time_To_GNAT_Time (T : OS_Time) return Time_Stamp_Type is
      GNAT_Time : Time_Stamp_Type;

      type Underlying_OS_Time is
        range -(2 ** 63) ..  +(2 ** 63 - 1);
      --  Underlying_OS_Time is a redeclaration of OS_Time to allow integer
      --  manipulation. Remove this in favor of To_Ada/To_C once newer
      --  GNAT releases are available with these functions.

      function To_Int is
        new Ada.Unchecked_Conversion (OS_Time, Underlying_OS_Time);
      function From_Int is
        new Ada.Unchecked_Conversion (Underlying_OS_Time, OS_Time);

      TI : Underlying_OS_Time := To_Int (T);
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      if T = Invalid_Time then
         return Empty_Time_Stamp;
      end if;

      if On_Windows and then TI mod 2 > 0 then
         --  Windows ALI files had timestamps rounded to even seconds
         --  historically. The rounding was originally done in GM_Split.
         --  Now that GM_Split no longer does it, we are rounding it here
         --  only for ALI files.

         TI := TI + 1;
      end if;

      GM_Split (From_Int (TI), Y, Mo, D, H, Mn, S);

      Make_Time_Stamp
        (Year    => Nat (Y),
         Month   => Nat (Mo),
         Day     => Nat (D),
         Hour    => Nat (H),
         Minutes => Nat (Mn),
         Seconds => Nat (S),
         TS      => GNAT_Time);

      return GNAT_Time;
   end OS_Time_To_GNAT_Time;

   -----------------
   -- Prep_Suffix --
   -----------------

   function Prep_Suffix return String is
   begin
      return ".prep";
   end Prep_Suffix;

   ------------------
   -- Program_Name --
   ------------------

   function Program_Name (Nam : String; Prog : String) return String_Access is
      End_Of_Prefix   : Natural := 0;
      Start_Of_Prefix : Positive := 1;
      Start_Of_Suffix : Positive;

   begin
      --  Get the name of the current program being executed

      Find_Program_Name;

      --  Find the target prefix if any, for the cross compilation case.
      --  For instance in "powerpc-elf-gcc" the target prefix is
      --  "powerpc-elf-"
      --  Ditto for suffix, e.g. in "gcc-4.1", the suffix is "-4.1"

      for J in reverse 1 .. Name_Len loop
         if Is_Directory_Separator (Name_Buffer (J))
           or else Name_Buffer (J) = ':'
         then
            Start_Of_Prefix := J + 1;
            exit;
         end if;
      end loop;

      --  Find End_Of_Prefix

      for J in Start_Of_Prefix .. Name_Len - Prog'Length + 1 loop
         if Name_Buffer (J .. J + Prog'Length - 1) = Prog then
            End_Of_Prefix := J - 1;
            exit;
         end if;
      end loop;

      Start_Of_Suffix := End_Of_Prefix + Prog'Length + 1;

      --  Create the new program name

      return new String'
        (Name_Buffer (Start_Of_Prefix .. End_Of_Prefix)
         & Nam
         & Name_Buffer (Start_Of_Suffix .. Name_Len));
   end Program_Name;

   ------------------------------
   -- Read_Default_Search_Dirs --
   ------------------------------

   function Read_Default_Search_Dirs
     (Search_Dir_Prefix       : String_Access;
      Search_File             : String_Access;
      Search_Dir_Default_Name : String_Access) return String_Access
   is
      Prefix_Len : constant Integer := Search_Dir_Prefix.all'Length;
      Buffer     : String (1 .. Prefix_Len + Search_File.all'Length + 1);
      File_FD    : File_Descriptor;
      S, S1      : String_Access;
      Len        : Integer;
      Curr       : Integer;
      Actual_Len : Integer;
      J1         : Integer;

      Prev_Was_Separator : Boolean;
      Nb_Relative_Dir    : Integer;

      function Is_Relative (S : String; K : Positive) return Boolean;
      pragma Inline (Is_Relative);
      --  Returns True if a relative directory specification is found
      --  in S at position K, False otherwise.

      -----------------
      -- Is_Relative --
      -----------------

      function Is_Relative (S : String; K : Positive) return Boolean is
      begin
         return not Is_Absolute_Path (S (K .. S'Last));
      end Is_Relative;

   --  Start of processing for Read_Default_Search_Dirs

   begin
      --  Construct a C compatible character string buffer

      Buffer (1 .. Search_Dir_Prefix.all'Length) :=
        Search_Dir_Prefix.all;
      Buffer (Search_Dir_Prefix.all'Length + 1 .. Buffer'Last - 1) :=
        Search_File.all;
      Buffer (Buffer'Last) := ASCII.NUL;

      File_FD := Open_Read (Buffer'Address, Binary);
      if File_FD = Invalid_FD then
         return Search_Dir_Default_Name;
      end if;

      Len := Integer (File_Length (File_FD));

      --  An extra character for a trailing Path_Separator is allocated

      S := new String (1 .. Len + 1);
      S (Len + 1) := Path_Separator;

      --  Read the file. Note that the loop is probably not necessary since the
      --  whole file is read at once but the loop is harmless and that way we
      --  are sure to accommodate systems where this is not the case.

      Curr := 1;
      Actual_Len := Len;
      while Actual_Len /= 0 loop
         Actual_Len := Read (File_FD, S (Curr)'Address, Len);
         Curr := Curr + Actual_Len;
      end loop;

      --  Process the file, dealing with path separators

      Prev_Was_Separator := True;
      Nb_Relative_Dir := 0;
      for J in 1 .. Len loop

         --  Treat any EOL character as a path separator. Note that we do
         --  not treat space as a path separator (we used to treat space as a
         --  path separator in an earlier version). That way space can appear
         --  as a legitimate character in a path name.

         if S (J) = ASCII.LF or else S (J) = ASCII.CR then
            S (J) := Path_Separator;
         end if;

         --  Test for explicit path separator (or control char as above)

         if S (J) = Path_Separator then
            Prev_Was_Separator := True;

         --  If not path separator, register use of relative directory

         else
            if Prev_Was_Separator and then Is_Relative (S.all, J) then
               Nb_Relative_Dir := Nb_Relative_Dir + 1;
            end if;

            Prev_Was_Separator := False;
         end if;
      end loop;

      if Nb_Relative_Dir = 0 then
         return S;
      end if;

      --  Add the Search_Dir_Prefix to all relative paths

      S1 := new String (1 .. S'Length + Nb_Relative_Dir * Prefix_Len);
      J1 := 1;
      Prev_Was_Separator := True;
      for J in 1 .. Len + 1 loop
         if S (J) = Path_Separator then
            Prev_Was_Separator := True;

         else
            if Prev_Was_Separator and then Is_Relative (S.all, J) then
               S1 (J1 .. J1 + Prefix_Len - 1) := Search_Dir_Prefix.all;
               J1 := J1 + Prefix_Len;
            end if;

            Prev_Was_Separator := False;
         end if;
         S1 (J1) := S (J);
         J1 := J1 + 1;
      end loop;

      Free (S);
      return S1;
   end Read_Default_Search_Dirs;

   -----------------------
   -- Read_Library_Info --
   -----------------------

   function Read_Library_Info
     (Lib_File  : File_Name_Type;
      Fatal_Err : Boolean := False) return Text_Buffer_Ptr
   is
      File : File_Name_Type;
      Attr : aliased File_Attributes;
   begin
      Find_File (Lib_File, Library, File, Attr'Access);
      return Read_Library_Info_From_Full
        (Full_Lib_File => File,
         Lib_File_Attr => Attr'Access,
         Fatal_Err     => Fatal_Err);
   end Read_Library_Info;

   ---------------------------------
   -- Read_Library_Info_From_Full --
   ---------------------------------

   function Read_Library_Info_From_Full
     (Full_Lib_File : File_Name_Type;
      Lib_File_Attr : access File_Attributes;
      Fatal_Err     : Boolean := False) return Text_Buffer_Ptr
   is
      Lib_FD : File_Descriptor;
      --  The file descriptor for the current library file. A negative value
      --  indicates failure to open the specified source file.

      Len : Integer;
      --  Length of source file text (ALI). If it doesn't fit in an integer
      --  we're probably stuck anyway (>2 gigs of source seems a lot, and
      --  there are other places in the compiler that make this assumption).

      Text : Text_Buffer_Ptr;
      --  Allocated text buffer

      Status : Boolean;
      pragma Warnings (Off, Status);
      --  For the calls to Close

   begin
      Current_Full_Lib_Name := Full_Lib_File;
      Current_Full_Obj_Name := Object_File_Name (Current_Full_Lib_Name);

      if Current_Full_Lib_Name = No_File then
         if Fatal_Err then
            Fail ("Cannot find: " & Name_Buffer (1 .. Name_Len));
         else
            Current_Full_Obj_Stamp := Empty_Time_Stamp;
            return null;
         end if;
      end if;

      Get_Name_String (Current_Full_Lib_Name);
      Name_Buffer (Name_Len + 1) := ASCII.NUL;

      --  Open the library FD, note that we open in binary mode, because as
      --  documented in the spec, the caller is expected to handle either
      --  DOS or Unix mode files, and there is no point in wasting time on
      --  text translation when it is not required.

      Lib_FD := Open_Read (Name_Buffer'Address, Binary);

      if Lib_FD = Invalid_FD then
         if Fatal_Err then
            Fail ("Cannot open: " & Name_Buffer (1 .. Name_Len));
         else
            Current_Full_Obj_Stamp := Empty_Time_Stamp;
            return null;
         end if;
      end if;

      --  Compute the length of the file (potentially also preparing other data
      --  like the timestamp and whether the file is read-only, for future use)

      Len := Integer (File_Length (Name_Buffer'Address, Lib_File_Attr));

      --  Check for object file consistency if requested

      if Opt.Check_Object_Consistency then
         --  On most systems, this does not result in an extra system call

         Current_Full_Lib_Stamp :=
           OS_Time_To_GNAT_Time
             (File_Time_Stamp (Name_Buffer'Address, Lib_File_Attr));

         --  ??? One system call here

         Current_Full_Obj_Stamp := File_Stamp (Current_Full_Obj_Name);

         if Current_Full_Obj_Stamp (1) = ' ' then

            --  When the library is readonly always assume object is consistent
            --  The call to Is_Writable_File only results in a system call on
            --  some systems, but in most cases it has already been computed as
            --  part of the call to File_Length above.

            Get_Name_String (Current_Full_Lib_Name);
            Name_Buffer (Name_Len + 1) := ASCII.NUL;

            if not Is_Writable_File (Name_Buffer'Address, Lib_File_Attr) then
               Current_Full_Obj_Stamp := Current_Full_Lib_Stamp;

            elsif Fatal_Err then
               Get_Name_String (Current_Full_Obj_Name);
               Close (Lib_FD, Status);

               --  No need to check the status, we fail anyway

               Fail ("Cannot find: " & Name_Buffer (1 .. Name_Len));

            else
               Current_Full_Obj_Stamp := Empty_Time_Stamp;
               Close (Lib_FD, Status);

               --  No need to check the status, we return null anyway

               return null;
            end if;

         elsif Current_Full_Obj_Stamp < Current_Full_Lib_Stamp then
            Close (Lib_FD, Status);

            --  No need to check the status, we return null anyway

            return null;
         end if;
      end if;

      --  Read data from the file

      declare
         Actual_Len : Integer;

         Lo : constant Text_Ptr := 0;
         --  Low bound for allocated text buffer

         Hi : Text_Ptr := Text_Ptr (Len);
         --  High bound for allocated text buffer. Note length is Len + 1
         --  which allows for extra EOF character at the end of the buffer.

      begin
         --  Allocate text buffer. Note extra character at end for EOF

         Text := new Text_Buffer (Lo .. Hi);

         --  Some systems have file types that require one read per line,
         --  so read until we get the Len bytes or until there are no more
         --  characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Lib_FD, Text (Hi)'Address, Len);
            Hi := Hi + Text_Ptr (Actual_Len);
            exit when Actual_Len = Len or else Actual_Len <= 0;
         end loop;

         Text (Hi) := EOF;
      end;

      --  Read is complete, close file and we are done

      Close (Lib_FD, Status);
      --  The status should never be False. But, if it is, what can we do?
      --  So, we don't test it.

      return Text;

   end Read_Library_Info_From_Full;

   ----------------------
   -- Read_Source_File --
   ----------------------

   procedure Read_Source_File
     (N   : File_Name_Type;
      Lo  : Source_Ptr;
      Hi  : out Source_Ptr;
      Src : out Source_Buffer_Ptr;
      FD  : out File_Descriptor;
      T   : File_Type := Source)
   is
      Len : Integer;
      --  Length of file, assume no more than 2 gigabytes of source

      Actual_Len : Integer;

      Status : Boolean;
      pragma Warnings (Off, Status);
      --  For the call to Close

   begin
      Current_Full_Source_Name  := Find_File (N, T, Full_Name => True);
      Current_Full_Source_Stamp := File_Stamp (Current_Full_Source_Name);

      if Current_Full_Source_Name = No_File then

         --  If we were trying to access the main file and we could not find
         --  it, we have an error.

         if N = Current_Main then
            Get_Name_String (N);
            Fail ("Cannot find: " & Name_Buffer (1 .. Name_Len));
         end if;

         FD  := Null_FD;
         Src := null;
         Hi  := No_Location;
         return;
      end if;

      Get_Name_String (Current_Full_Source_Name);
      Name_Buffer (Name_Len + 1) := ASCII.NUL;

      --  Open the source FD, note that we open in binary mode, because as
      --  documented in the spec, the caller is expected to handle either
      --  DOS or Unix mode files, and there is no point in wasting time on
      --  text translation when it is not required.

      FD := Open_Read (Name_Buffer'Address, Binary);

      if FD = Invalid_FD then
         Src := null;
         Hi  := No_Location;
         return;
      end if;

      --  If it's a Source file, print out the file name, if requested, and if
      --  it's not part of the runtimes, store it in File_Name_Chars. We don't
      --  want to print non-Source files, like GNAT-TEMP-000001.TMP used to
      --  pass information from gprbuild to gcc. We don't want to save runtime
      --  file names, because we don't want users to send them in bug reports.

      if T = Source then
         declare
            Name : String renames Name_Buffer (1 .. Name_Len);
            Inc  : String renames Include_Dir_Default_Prefix.all;

            Part_Of_Runtimes : constant Boolean :=
              Inc /= ""
                and then Inc'Length < Name_Len
                and then Name_Buffer (1 .. Inc'Length) = Inc;

         begin
            if Debug.Debug_Flag_Dot_N then
               Write_Line (Name);
            end if;

            if not Part_Of_Runtimes then
               File_Name_Chars.Append_All (File_Name_Chars.Table_Type (Name));
               File_Name_Chars.Append (ASCII.LF);
            end if;
         end;
      end if;

      --  Prepare to read data from the file

      Len := Integer (File_Length (FD));

      --  Set Hi so that length is one more than the physical length,
      --  allowing for the extra EOF character at the end of the buffer

      Hi := Lo + Source_Ptr (Len);

      --  Do the actual read operation

      declare
         Var_Ptr : constant Source_Buffer_Ptr_Var :=
           new Source_Buffer (Lo .. Hi);
         --  Allocate source buffer, allowing extra character at end for EOF
      begin
         --  Some systems have file types that require one read per line,
         --  so read until we get the Len bytes or until there are no more
         --  characters.

         Hi := Lo;
         loop
            Actual_Len := Read (FD, Var_Ptr (Hi)'Address, Len);
            Hi := Hi + Source_Ptr (Actual_Len);
            exit when Actual_Len = Len or else Actual_Len <= 0;
         end loop;

         Var_Ptr (Hi) := EOF;
         Src := Var_Ptr.all'Access;
      end;

      --  Read is complete, get time stamp and close file and we are done

      Close (FD, Status);

      --  The status should never be False. But, if it is, what can we do?
      --  So, we don't test it.

      --  ???We don't really need to return Hi anymore; We could get rid of
      --  it. We could also make this into a function.

      pragma Assert (Hi = Src'Last);
   end Read_Source_File;

   -------------------
   -- Relocate_Path --
   -------------------

   function Relocate_Path
     (Prefix : String;
      Path   : String) return String_Ptr
   is
      S : String_Ptr;

      procedure set_std_prefix (S : String; Len : Integer);
      pragma Import (C, set_std_prefix);

   begin
      if Std_Prefix = null then
         Std_Prefix := String_Ptr (Getenv ("GNSA_ROOT"));

         if Std_Prefix.all = "" then
            Std_Prefix := Executable_Prefix;

         elsif not Is_Directory_Separator (Std_Prefix (Std_Prefix'Last)) then

            --  The remainder of this function assumes that Std_Prefix
            --  terminates with a dir separator, so we force this here.

            declare
               Old_Prefix : String_Ptr := Std_Prefix;
            begin
               Std_Prefix := new String (1 .. Old_Prefix'Length + 1);
               Std_Prefix (1 .. Old_Prefix'Length) := Old_Prefix.all;
               Std_Prefix (Old_Prefix'Length + 1) := Directory_Separator;
               Free (Old_Prefix);
            end;
         end if;

         if Std_Prefix.all /= "" then

            --  Remove trailing directory separator when calling set_std_prefix

            set_std_prefix (Std_Prefix.all, Std_Prefix'Length - 1);
         end if;
      end if;

      if Path'Last >= Prefix'Last and then Path (Prefix'Range) = Prefix then
         if Std_Prefix.all /= "" then
            S := new String
              (1 .. Std_Prefix'Length + Path'Last - Prefix'Last);
            S (1 .. Std_Prefix'Length) := Std_Prefix.all;
            S (Std_Prefix'Length + 1 .. S'Last) :=
              Path (Prefix'Last + 1 .. Path'Last);
            return S;
         end if;
      end if;

      return new String'(Path);
   end Relocate_Path;

   -----------------
   -- Set_Program --
   -----------------

   procedure Set_Program (P : Program_Type) is
   begin
      if Program_Set then
         Fail ("Set_Program called twice");
      end if;

      Program_Set := True;
      Running_Program := P;
   end Set_Program;

   ----------------
   -- Shared_Lib --
   ----------------

   function Shared_Lib (Name : String) return String is
      Library : String (1 .. Name'Length + Library_Version'Length + 3);
      --  3 = 2 for "-l" + 1 for "-" before lib version

   begin
      Library (1 .. 2)                          := "-l";
      Library (3 .. 2 + Name'Length)            := Name;
      Library (3 + Name'Length)                 := '-';
      Library (4 + Name'Length .. Library'Last) := Library_Version;
      return Library;
   end Shared_Lib;

   ----------------------
   -- Smart_File_Stamp --
   ----------------------

   function Smart_File_Stamp
     (N : File_Name_Type;
      T : File_Type) return Time_Stamp_Type
   is
      File : File_Name_Type;
      Attr : aliased File_Attributes;

   begin
      if not File_Cache_Enabled then
         Find_File (N, T, File, Attr'Access);
      else
         Smart_Find_File (N, T, File, Attr);
      end if;

      if File = No_File then
         return Empty_Time_Stamp;
      else
         Get_Name_String (File);
         Name_Buffer (Name_Len + 1) := ASCII.NUL;
         return
           OS_Time_To_GNAT_Time
             (File_Time_Stamp (Name_Buffer'Address, Attr'Access));
      end if;
   end Smart_File_Stamp;

   ---------------------
   -- Smart_Find_File --
   ---------------------

   function Smart_Find_File
     (N : File_Name_Type;
      T : File_Type) return File_Name_Type
   is
      File : File_Name_Type;
      Attr : File_Attributes;
   begin
      Smart_Find_File (N, T, File, Attr);
      return File;
   end Smart_Find_File;

   ---------------------
   -- Smart_Find_File --
   ---------------------

   procedure Smart_Find_File
     (N     : File_Name_Type;
      T     : File_Type;
      Found : out File_Name_Type;
      Attr  : out File_Attributes)
   is
      Info : File_Info_Cache;

   begin
      if not File_Cache_Enabled then
         Find_File (N, T, Info.File, Info.Attr'Access);

      else
         Info := File_Name_Hash_Table.Get (N);

         if Info.File = No_File then
            Find_File (N, T, Info.File, Info.Attr'Access);
            File_Name_Hash_Table.Set (N, Info);
         end if;
      end if;

      Found := Info.File;
      Attr  := Info.Attr;
   end Smart_Find_File;

   ----------------------
   -- Source_File_Data --
   ----------------------

   procedure Source_File_Data (Cache : Boolean) is
   begin
      File_Cache_Enabled := Cache;
   end Source_File_Data;

   -----------------------
   -- Source_File_Stamp --
   -----------------------

   function Source_File_Stamp (N : File_Name_Type) return Time_Stamp_Type is
   begin
      return Smart_File_Stamp (N, Source);
   end Source_File_Stamp;

   ---------------------
   -- Strip_Directory --
   ---------------------

   function Strip_Directory (Name : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Name);

      for J in reverse 1 .. Name_Len - 1 loop

         --  If we find the last directory separator

         if Is_Directory_Separator (Name_Buffer (J)) then

            --  Return part of Name that follows this last directory separator

            Name_Buffer (1 .. Name_Len - J) := Name_Buffer (J + 1 .. Name_Len);
            Name_Len := Name_Len - J;
            return Name_Find;
         end if;
      end loop;

      --  There were no directory separator, just return Name

      return Name;
   end Strip_Directory;

   ------------------
   -- Strip_Suffix --
   ------------------

   function Strip_Suffix (Name : File_Name_Type) return File_Name_Type is
   begin
      Get_Name_String (Name);

      for J in reverse 2 .. Name_Len loop

         --  If we found the last '.', return part of Name that precedes it

         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            return Name_Enter;
         end if;
      end loop;

      return Name;
   end Strip_Suffix;

   ---------------------------
   -- To_Canonical_File_List --
   ---------------------------

   function To_Canonical_File_List
     (Wildcard_Host_File : String;
      Only_Dirs          : Boolean) return String_Access_List_Access
   is
      function To_Canonical_File_List_Init
        (Host_File : Address;
         Only_Dirs : Integer) return Integer;
      pragma Import (C, To_Canonical_File_List_Init,
                     "__gnat_to_canonical_file_list_init");

      function To_Canonical_File_List_Next return Address;
      pragma Import (C, To_Canonical_File_List_Next,
                     "__gnat_to_canonical_file_list_next");

      procedure To_Canonical_File_List_Free;
      pragma Import (C, To_Canonical_File_List_Free,
                     "__gnat_to_canonical_file_list_free");

      Num_Files            : Integer;
      C_Wildcard_Host_File : String (1 .. Wildcard_Host_File'Length + 1);

   begin
      C_Wildcard_Host_File (1 .. Wildcard_Host_File'Length) :=
        Wildcard_Host_File;
      C_Wildcard_Host_File (C_Wildcard_Host_File'Last) := ASCII.NUL;

      --  Do the expansion and say how many there are

      Num_Files := To_Canonical_File_List_Init
         (C_Wildcard_Host_File'Address, Boolean'Pos (Only_Dirs));

      declare
         Canonical_File_List : String_Access_List (1 .. Num_Files);
         Canonical_File_Addr : Address;
         Canonical_File_Len  : CRTL.size_t;

      begin
         --  Retrieve the expanded directory names and build the list

         for J in 1 .. Num_Files loop
            Canonical_File_Addr := To_Canonical_File_List_Next;
            Canonical_File_Len  := C_String_Length (Canonical_File_Addr);
            Canonical_File_List (J) := To_Path_String_Access
                  (Canonical_File_Addr, Canonical_File_Len);
         end loop;

         --  Free up the storage

         To_Canonical_File_List_Free;

         return new String_Access_List'(Canonical_File_List);
      end;
   end To_Canonical_File_List;

   ----------------------
   -- To_Host_Dir_Spec --
   ----------------------

   function To_Host_Dir_Spec
     (Canonical_Dir : String;
      Prefix_Style  : Boolean) return String_Access
   is
      function To_Host_Dir_Spec
        (Canonical_Dir : Address;
         Prefix_Flag   : Integer) return Address;
      pragma Import (C, To_Host_Dir_Spec, "__gnat_to_host_dir_spec");

      C_Canonical_Dir : String (1 .. Canonical_Dir'Length + 1);
      Host_Dir_Addr   : Address;
      Host_Dir_Len    : CRTL.size_t;

   begin
      C_Canonical_Dir (1 .. Canonical_Dir'Length) := Canonical_Dir;
      C_Canonical_Dir (C_Canonical_Dir'Last)      := ASCII.NUL;

      if Prefix_Style then
         Host_Dir_Addr := To_Host_Dir_Spec (C_Canonical_Dir'Address, 1);
      else
         Host_Dir_Addr := To_Host_Dir_Spec (C_Canonical_Dir'Address, 0);
      end if;
      Host_Dir_Len := C_String_Length (Host_Dir_Addr);

      if Host_Dir_Len = 0 then
         return null;
      else
         return To_Path_String_Access (Host_Dir_Addr, Host_Dir_Len);
      end if;
   end To_Host_Dir_Spec;

   -----------------------
   -- To_Host_File_Spec --
   -----------------------

   function To_Host_File_Spec
     (Canonical_File : String) return String_Access
   is
      function To_Host_File_Spec (Canonical_File : Address) return Address;
      pragma Import (C, To_Host_File_Spec, "__gnat_to_host_file_spec");

      C_Canonical_File      : String (1 .. Canonical_File'Length + 1);
      Host_File_Addr : Address;
      Host_File_Len  : CRTL.size_t;

   begin
      C_Canonical_File (1 .. Canonical_File'Length) := Canonical_File;
      C_Canonical_File (C_Canonical_File'Last)      := ASCII.NUL;

      Host_File_Addr := To_Host_File_Spec (C_Canonical_File'Address);
      Host_File_Len  := C_String_Length (Host_File_Addr);

      if Host_File_Len = 0 then
         return null;
      else
         return To_Path_String_Access
                  (Host_File_Addr, Host_File_Len);
      end if;
   end To_Host_File_Spec;

   ---------------------------
   -- To_Path_String_Access --
   ---------------------------

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : CRTL.size_t) return String_Access
   is
      subtype Path_String is String (1 .. Integer (Path_Len));
      type Path_String_Access is access Path_String;

      function Address_To_Access is new
        Ada.Unchecked_Conversion (Source => Address,
                                  Target => Path_String_Access);

      Path_Access : constant Path_String_Access :=
                      Address_To_Access (Path_Addr);

      Return_Val : String_Access;

   begin
      Return_Val := new String (1 .. Integer (Path_Len));

      for J in 1 .. Integer (Path_Len) loop
         Return_Val (J) := Path_Access (J);
      end loop;

      return Return_Val;
   end To_Path_String_Access;

   -----------------
   -- Update_Path --
   -----------------

   function Update_Path (Path : String_Ptr) return String_Ptr is

      function C_Update_Path (Path, Component : Address) return Address;
      pragma Import (C, C_Update_Path, "update_path");

      In_Length      : constant Integer := Path'Length;
      In_String      : String (1 .. In_Length + 1);
      Component_Name : aliased String := "GCC" & ASCII.NUL;
      Result_Ptr     : Address;
      Result_Length  : CRTL.size_t;
      Out_String     : String_Ptr;

   begin
      In_String (1 .. In_Length) := Path.all;
      In_String (In_Length + 1) := ASCII.NUL;
      Result_Ptr := C_Update_Path (In_String'Address, Component_Name'Address);
      Result_Length := CRTL.strlen (Result_Ptr);

      Out_String := new String (1 .. Integer (Result_Length));
      CRTL.strncpy (Out_String.all'Address, Result_Ptr, Result_Length);
      return Out_String;
   end Update_Path;

   ----------------
   -- Write_Info --
   ----------------

   procedure Write_Info (Info : String) is
   begin
      Write_With_Check (Info'Address, Info'Length);
      Write_With_Check (EOL'Address, 1);
   end Write_Info;

   ------------------------
   -- Write_Program_Name --
   ------------------------

   procedure Write_Program_Name is
      Save_Buffer : constant String (1 .. Name_Len) :=
                      Name_Buffer (1 .. Name_Len);

   begin
      Find_Program_Name;

      --  Convert the name to lower case so error messages are the same on
      --  all systems.

      for J in 1 .. Name_Len loop
         if Name_Buffer (J) in 'A' .. 'Z' then
            Name_Buffer (J) :=
              Character'Val (Character'Pos (Name_Buffer (J)) + 32);
         end if;
      end loop;

      Write_Str (Name_Buffer (1 .. Name_Len));

      --  Restore Name_Buffer which was clobbered by the call to
      --  Find_Program_Name

      Name_Len := Save_Buffer'Last;
      Name_Buffer (1 .. Name_Len) := Save_Buffer;
   end Write_Program_Name;

   ----------------------
   -- Write_With_Check --
   ----------------------

   procedure Write_With_Check (A : Address; N : Integer) is
      Ignore : Boolean;
   begin
      if N = Write (Output_FD, A, N) then
         return;
      else
         Write_Str ("error: disk full writing ");
         Write_Name_Decoded (Output_File_Name);
         Write_Eol;
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ASCII.NUL;
         Delete_File (Name_Buffer'Address, Ignore);
         Exit_Program (E_Fatal);
      end if;
   end Write_With_Check;

----------------------------
-- Package Initialization --
----------------------------

   procedure Reset_File_Attributes (Attr : System.Address);
   pragma Import (C, Reset_File_Attributes, "__gnat_reset_attributes");

begin
   Initialization : declare

      function Get_Default_Identifier_Character_Set return Character;
      pragma Import (C, Get_Default_Identifier_Character_Set,
                       "__gnat_get_default_identifier_character_set");
      --  Function to determine the default identifier character set,
      --  which is system dependent. See Opt package spec for a list of
      --  the possible character codes and their interpretations.

      function Get_Maximum_File_Name_Length return Int;
      pragma Import (C, Get_Maximum_File_Name_Length,
                    "__gnat_get_maximum_file_name_length");
      --  Function to get maximum file name length for system

      Sizeof_File_Attributes : Integer;
      pragma Import (C, Sizeof_File_Attributes,
                     "__gnat_size_of_file_attributes");

   begin
      pragma Assert (Sizeof_File_Attributes <= File_Attributes_Size);

      Reset_File_Attributes (Unknown_Attributes'Address);

      Identifier_Character_Set := Get_Default_Identifier_Character_Set;
      Maximum_File_Name_Length := Get_Maximum_File_Name_Length;

      --  Following should be removed by having above function return
      --  Integer'Last as indication of no maximum instead of -1 ???

      if Maximum_File_Name_Length = -1 then
         Maximum_File_Name_Length := Int'Last;
      end if;

      Src_Search_Directories.Set_Last (Primary_Directory);
      Src_Search_Directories.Table (Primary_Directory) := new String'("");

      Lib_Search_Directories.Set_Last (Primary_Directory);
      Lib_Search_Directories.Table (Primary_Directory) := new String'("");

      Osint.Initialize;
   end Initialization;

end Osint;
