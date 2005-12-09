------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                O S I N T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Fmap;     use Fmap;
with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Sdefault; use Sdefault;
with Table;
with Targparm; use Targparm;

with System.Case_Util; use System.Case_Util;

with Unchecked_Conversion;

with GNAT.HTable;

package body Osint is

   Running_Program : Program_Type := Unspecified;
   --  comment required here ???

   Program_Set : Boolean := False;
   --  comment required here ???

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
   --  that are used to locate the actual file and for the purpose of
   --  message construction. These names need not be accessible by Name_Find,
   --  and can be therefore created by using routine Name_Enter. The files in
   --  question are file names with a prefix directory (ie the files not
   --  in the current directory). File names without a prefix directory are
   --  entered with Name_Find because special values might be attached to
   --  the various Info fields of the corresponding name table entry.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Append_Suffix_To_File_Name
     (Name   : Name_Id;
      Suffix : String) return Name_Id;
   --  Appends Suffix to Name and returns the new name

   function OS_Time_To_GNAT_Time (T : OS_Time) return Time_Stamp_Type;
   --  Convert OS format time to GNAT format time stamp

   function Concat (String_One : String; String_Two : String) return String;
   --  Concatenates 2 strings and returns the result of the concatenation

   function Executable_Prefix return String_Ptr;
   --  Returns the name of the root directory where the executable is stored.
   --  The executable must be located in a directory called "bin", or
   --  under root/lib/gcc-lib/..., or under root/libexec/gcc/... Thus, if
   --  the executable is stored in directory "/foo/bar/bin", this routine
   --  returns "/foo/bar/".  Return "" if the location is not recognized
   --  as described above.

   function Update_Path (Path : String_Ptr) return String_Ptr;
   --  Update the specified path to replace the prefix with the location
   --  where GNAT is installed. See the file prefix.c in GCC for details.

   procedure Write_With_Check (A : Address; N  : Integer);
   --  Writes N bytes from buffer starting at address A to file whose FD is
   --  stored in Output_FD, and whose file name is stored as a File_Name_Type
   --  in Output_File_Name. A check is made for disk full, and if this is
   --  detected, the file being written is deleted, and a fatal error is
   --  signalled.

   function Locate_File
     (N    : File_Name_Type;
      T    : File_Type;
      Dir  : Natural;
      Name : String) return File_Name_Type;
   --  See if the file N whose name is Name exists in directory Dir. Dir is
   --  an index into the Lib_Search_Directories table if T = Library.
   --  Otherwise if T = Source, Dir is an index into the
   --  Src_Search_Directories table. Returns the File_Name_Type of the
   --  full file name if file found, or No_File if not found.

   function C_String_Length (S : Address) return Integer;
   --  Returns length of a C string. Returns zero for a null address

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access;
   --  Converts a C String to an Ada String. Are we doing this to avoid
   --  withing Interfaces.C.Strings ???

   ------------------------------
   -- Other Local Declarations --
   ------------------------------

   EOL : constant Character := ASCII.LF;
   --  End of line character

   Number_File_Names : Int := 0;
   --  The total number of file names found on command line and placed in
   --  File_Names.

   Look_In_Primary_Directory_For_Current_Main : Boolean := False;
   --  When this variable is True, Find_File will only look in
   --  the Primary_Directory for the Current_Main file.
   --  This variable is always True for the compiler.
   --  It is also True for gnatmake, when the soucr name given
   --  on the command line has directory information.

   Current_Full_Source_Name  : File_Name_Type  := No_File;
   Current_Full_Source_Stamp : Time_Stamp_Type := Empty_Time_Stamp;
   Current_Full_Lib_Name     : File_Name_Type  := No_File;
   Current_Full_Lib_Stamp    : Time_Stamp_Type := Empty_Time_Stamp;
   Current_Full_Obj_Name     : File_Name_Type  := No_File;
   Current_Full_Obj_Stamp    : Time_Stamp_Type := Empty_Time_Stamp;
   --  Respectively full name (with directory info) and time stamp of
   --  the latest source, library and object files opened by Read_Source_File
   --  and Read_Library_Info.

   ------------------
   -- Search Paths --
   ------------------

   Primary_Directory : constant := 0;
   --  This is index in the tables created below for the first directory to
   --  search in for source or library information files. This is the
   --  directory containing the latest main input file (a source file for
   --  the compiler or a library file for the binder).

   package Src_Search_Directories is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => Primary_Directory,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Osint.Src_Search_Directories");
   --  Table of names of directories in which to search for source (Compiler)
   --  files. This table is filled in the order in which the directories are
   --  to be searched, and then used in that order.

   package Lib_Search_Directories is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Natural,
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
   --  he is guaranteed that the price to retrieve the full name (ie with
   --  directory info) or time stamp of the file will be payed only once,
   --  the first time the full name is actually searched (or the first time
   --  the time stamp is actually retrieved). This is achieved by employing
   --  a hash table that stores as a key the File_Name_Type of the file and
   --  associates to that File_Name_Type the full file name of the file and its
   --  time stamp.

   File_Cache_Enabled : Boolean := False;
   --  Set to true if you want the enable the file data caching mechanism

   type File_Hash_Num is range 0 .. 1020;

   function File_Hash (F : File_Name_Type) return File_Hash_Num;
   --  Compute hash index for use by Simple_HTable

   package File_Name_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => File_Hash_Num,
     Element    => File_Name_Type,
     No_Element => No_File,
     Key        => File_Name_Type,
     Hash       => File_Hash,
     Equal      => "=");

   package File_Stamp_Hash_Table is new GNAT.HTable.Simple_HTable (
     Header_Num => File_Hash_Num,
     Element    => Time_Stamp_Type,
     No_Element => Empty_Time_Stamp,
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
   --  Takes the same parameter as the routine above (N is a file name
   --  without any prefix directory information) and behaves like File_Stamp
   --  except that if File_Cache_Enabled is True this routine looks first in
   --  the hash table to see if the file stamp of the file is already
   --  available.

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
      --  HKEY_LOCAL_MACHINE\SOFTWARE\Ada Core Technologies\
      --                             GNAT\Standard Libraries
      --  Return an empty string on other systems

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
         Buffer     : String (1 .. Path_File_Name'Length + 1);
         Len        : Natural;
         Actual_Len : Natural;
         S          : String_Access;
         Curr       : Natural;
         First      : Natural;
         Ch         : Character;

         Status : Boolean;
         --  For the call to Close

      begin
         --  Construct a C compatible character string buffer

         Buffer (1 .. Buffer'Last - 1) := Path_File_Name.all;
         Buffer (Buffer'Last) := ASCII.NUL;

         File_FD := Open_Read (Buffer'Address, Binary);

         --  If we cannot open the file, we ignore it, we don't fail

         if File_FD = Invalid_FD then
            return;
         end if;

         Len := Integer (File_Length (File_FD));

         S := new String (1 .. Len);

         --  Read the file. Note that the loop is not necessary since the
         --  whole file is read at once except on VMS.

         Curr := 1;
         Actual_Len := Len;
         while Curr <= Len and then Actual_Len /= 0 loop
            Actual_Len := Read (File_FD, S (Curr)'Address, Len);
            Curr := Curr + Actual_Len;
         end loop;

         --  We are done with the file, so we close it

         Close (File_FD, Status);
         --  We ignore any error here, because we have successfully read the
         --  file.

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
         function Strlen (Str : Address) return Integer;
         pragma Import (C, Strlen, "strlen");
         procedure Strncpy (X : Address; Y : Address; Length : Integer);
         pragma Import (C, Strncpy, "strncpy");
         Result_Ptr : Address;
         Result_Length : Integer;
         Out_String : String_Ptr;

      begin
         Result_Ptr := C_Get_Libraries_From_Registry;
         Result_Length := Strlen (Result_Ptr);

         Out_String := new String (1 .. Result_Length);
         Strncpy (Out_String.all'Address, Result_Ptr, Result_Length);
         return Out_String;
      end Get_Libraries_From_Registry;

   --  Start of processing for Add_Default_Search_Dirs

   begin
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

      --  On VMS, don't expand the logical name (e.g. environment variable),
      --  just put it into Unix (e.g. canonical) format. System services
      --  will handle the expansion as part of the file processing.

      for Additional_Source_Dir in False .. True loop

         if Additional_Source_Dir then
            Search_Path := Getenv (Ada_Include_Path);
            if Search_Path'Length > 0 then
               if Hostparm.OpenVMS then
                  Search_Path := To_Canonical_Path_Spec ("ADA_INCLUDE_PATH:");
               else
                  Search_Path := To_Canonical_Path_Spec (Search_Path.all);
               end if;
            end if;
         else
            Search_Path := Getenv (Ada_Objects_Path);
            if Search_Path'Length > 0 then
               if Hostparm.OpenVMS then
                  Search_Path := To_Canonical_Path_Spec ("ADA_OBJECTS_PATH:");
               else
                  Search_Path := To_Canonical_Path_Spec (Search_Path.all);
               end if;
            end if;
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

      if RTS_Src_Path_Name /= null and then
         RTS_Lib_Path_Name /= null
      then
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

         if not Opt.No_Stdlib and not Opt.RTS_Switch then
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

      --  As Add_File may be called for mains specified inside
      --  a project file, File_Names may be too short and needs
      --  to be extended.

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

      Lib_Search_Directories.Increment_Last;
      Lib_Search_Directories.Table (Lib_Search_Directories.Last) :=
        Normalize_Directory_Name (Dir);
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
     (Name   : Name_Id;
      Suffix : String) return Name_Id
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

   function C_String_Length (S : Address) return Integer is
      function Strlen (S : Address) return Integer;
      pragma Import (C, Strlen, "strlen");
   begin
      if S = Null_Address then
         return 0;
      else
         return Strlen (S);
      end if;
   end C_String_Length;

   ------------------------------
   -- Canonical_Case_File_Name --
   ------------------------------

   --  For now, we only deal with the case of a-z. Eventually we should
   --  worry about other Latin-1 letters on systems that support this ???

   procedure Canonical_Case_File_Name (S : in out String) is
   begin
      if not File_Names_Case_Sensitive then
         for J in S'Range loop
            if S (J) in 'A' .. 'Z' then
               S (J) := Character'Val (
                          Character'Pos (S (J)) +
                          Character'Pos ('a')   -
                          Character'Pos ('A'));
            end if;
         end loop;
      end if;
   end Canonical_Case_File_Name;

   ------------
   -- Concat --
   ------------

   function Concat (String_One : String; String_Two : String) return String is
      Buffer : String (1 .. String_One'Length + String_Two'Length);
   begin
      Buffer (1 .. String_One'Length) := String_One;
      Buffer (String_One'Length + 1 .. Buffer'Last) := String_Two;
      return Buffer;
   end Concat;

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
         Fail ("Cannot create: ", Name_Buffer (1 .. Name_Len));
      end if;
   end Create_File_And_Check;

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

   ---------------------
   -- Executable_Name --
   ---------------------

   function Executable_Name (Name : File_Name_Type) return File_Name_Type is
      Exec_Suffix : String_Access;

   begin
      if Name = No_File then
         return No_File;
      end if;

      Get_Name_String (Name);
      Exec_Suffix := Get_Executable_Suffix;

      for J in Exec_Suffix'Range loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Exec_Suffix (J);
      end loop;

      Free (Exec_Suffix);

      return Name_Enter;
   end Executable_Name;

   -----------------------
   -- Executable_Prefix --
   -----------------------

   function Executable_Prefix return String_Ptr is
      function Get_Install_Dir (Exec : String) return String_Ptr;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (Exec : String) return String_Ptr is
      begin
         for J in reverse Exec'Range loop
            if Is_Directory_Separator (Exec (J)) then
               if J < Exec'Last - 5 then
                  if (To_Lower (Exec (J + 1)) = 'l'
                      and then To_Lower (Exec (J + 2)) = 'i'
                      and then To_Lower (Exec (J + 3)) = 'b')
                    or else
                      (To_Lower (Exec (J + 1)) = 'b'
                       and then To_Lower (Exec (J + 2)) = 'i'
                       and then To_Lower (Exec (J + 3)) = 'n')
                  then
                     return new String'(Exec (Exec'First .. J));
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

      return Get_Install_Dir
        (GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name.all).all);
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

   procedure Fail (S1 : String; S2 : String := ""; S3 : String := "") is
   begin
      --  We use Output in case there is a special output set up.
      --  In this case Set_Standard_Error will have no immediate effect.

      Set_Standard_Error;
      Osint.Write_Program_Name;
      Write_Str (": ");
      Write_Str (S1);
      Write_Str (S2);
      Write_Str (S3);
      Write_Eol;

      Exit_Program (E_Fatal);
   end Fail;

   ---------------
   -- File_Hash --
   ---------------

   function File_Hash (F : File_Name_Type) return File_Hash_Num is
   begin
      return File_Hash_Num (Int (F) rem File_Hash_Num'Range_Length);
   end File_Hash;

   ----------------
   -- File_Stamp --
   ----------------

   function File_Stamp (Name : File_Name_Type) return Time_Stamp_Type is
   begin
      if Name = No_File then
         return Empty_Time_Stamp;
      end if;

      Get_Name_String (Name);

      if not Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
         return Empty_Time_Stamp;
      else
         Name_Buffer (Name_Len + 1) := ASCII.NUL;
         return OS_Time_To_GNAT_Time (File_Time_Stamp (Name_Buffer));
      end if;
   end File_Stamp;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (N : File_Name_Type;
      T : File_Type) return File_Name_Type
   is
   begin
      Get_Name_String (N);

      declare
         File_Name : String renames Name_Buffer (1 .. Name_Len);
         File      : File_Name_Type := No_File;
         Last_Dir  : Natural;

      begin
         --  If we are looking for a config file, look only in the current
         --  directory, i.e. return input argument unchanged. Also look
         --  only in the current directory if we are looking for a .dg
         --  file (happens in -gnatD mode)

         if T = Config
           or else (Debug_Generated_Code
                      and then Name_Len > 3
                      and then
                      (Name_Buffer (Name_Len - 2 .. Name_Len) = ".dg"
                       or else
                       (Hostparm.OpenVMS and then
                        Name_Buffer (Name_Len - 2 .. Name_Len) = "_dg")))
         then
            return N;

         --  If we are trying to find the current main file just look in the
         --  directory where the user said it was.

         elsif Look_In_Primary_Directory_For_Current_Main
           and then Current_Main = N
         then
            return Locate_File (N, T, Primary_Directory, File_Name);

         --  Otherwise do standard search for source file

         else
            --  Check the mapping of this file name

            File := Mapped_Path_Name (N);

            --  If the file name is mapped to a path name, return the
            --  corresponding path name

            if File /= No_File then
               --  For locally removed file, Error_Name is returned; then
               --  return No_File, indicating the file is not a source.

               if File = Error_Name then
                  return No_File;

               else
                  return File;
               end if;
            end if;

            --  First place to look is in the primary directory (i.e. the same
            --  directory as the source) unless this has been disabled with -I-

            if Opt.Look_In_Primary_Dir then
               File := Locate_File (N, T, Primary_Directory, File_Name);

               if File /= No_File then
                  return File;
               end if;
            end if;

            --  Finally look in directories specified with switches -I/-aI/-aO

            if T = Library then
               Last_Dir := Lib_Search_Directories.Last;
            else
               Last_Dir := Src_Search_Directories.Last;
            end if;

            for D in Primary_Directory + 1 .. Last_Dir loop
               File := Locate_File (N, T, D, File_Name);

               if File /= No_File then
                  return File;
               end if;
            end loop;

            return No_File;
         end if;
      end;
   end Find_File;

   -----------------------
   -- Find_Program_Name --
   -----------------------

   procedure Find_Program_Name is
      Command_Name : String (1 .. Len_Arg (0));
      Cindex1 : Integer := Command_Name'First;
      Cindex2 : Integer := Command_Name'Last;

   begin
      Fill_Arg (Command_Name'Address, 0);

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

      --  Strip off any versioning information such as found on VMS.
      --  This would take the form of TOOL.exe followed by a ";" or "."
      --  and a sequence of one or more numbers.

      if Command_Name (Cindex2) in '0' .. '9' then
         for J in reverse Cindex1 .. Cindex2 loop
            if Command_Name (J) = '.' or Command_Name (J) = ';' then
               Cindex2 := J - 1;
               exit;
            end if;

            exit when Command_Name (J) not in '0' .. '9';
         end loop;
      end if;

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

   function Full_Lib_File_Name (N : File_Name_Type) return File_Name_Type is
   begin
      return Find_File (N, Library);
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

      if Search_Dir (Search_Dir'Last) /= Directory_Separator then
         Local_Search_Dir := new String'
           (Concat (Search_Dir, String'(1 => Directory_Separator)));
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

      Norm_Search_Dir := To_Canonical_Path_Spec (Local_Search_Dir.all);

      if Is_Absolute_Path (Norm_Search_Dir.all) then

         --  We first verify if there is a directory Include_Search_Dir
         --  containing default search directories

         Result_Search_Dir
           := Read_Default_Search_Dirs (Norm_Search_Dir,
                                        Search_File,
                                        null);
         Default_Search_Dir := new String'
           (Concat (Norm_Search_Dir.all, Default_Suffix_Dir.all));
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

            if Buffer (Path_Len) /= Directory_Separator then
               Path_Len := Path_Len + 1;
               Buffer (Path_Len) := Directory_Separator;
            end if;

            Current_Dir := new String'(Buffer (1 .. Path_Len));
         end;

         Norm_Search_Dir :=
           new String'(Concat (Current_Dir.all, Local_Search_Dir.all));

         Result_Search_Dir :=
           Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);

         Default_Search_Dir :=
           new String'
             (Concat (Norm_Search_Dir.all, Default_Suffix_Dir.all));

         Free (Norm_Search_Dir);

         if Result_Search_Dir /= null then
            return String_Ptr (Result_Search_Dir);

         elsif Is_Directory (Default_Search_Dir.all) then
            return String_Ptr (Default_Search_Dir);

         else
            --  Search in Search_Dir_Prefix/Search_Dir

            Norm_Search_Dir :=
              new String'
              (Concat (Update_Path (Search_Dir_Prefix).all,
                       Local_Search_Dir.all));

            Result_Search_Dir :=
              Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);

            Default_Search_Dir :=
              new String'
                (Concat (Norm_Search_Dir.all, Default_Suffix_Dir.all));

            Free (Norm_Search_Dir);

            if Result_Search_Dir /= null then
               return String_Ptr (Result_Search_Dir);

            elsif Is_Directory (Default_Search_Dir.all) then
               return String_Ptr (Default_Search_Dir);

            else
               --  We finally search in Search_Dir_Prefix/rts-Search_Dir

               Temp_String :=
                 new String'
                 (Concat (Update_Path (Search_Dir_Prefix).all, "rts-"));

               Norm_Search_Dir :=
                 new String'(Concat (Temp_String.all, Local_Search_Dir.all));

               Result_Search_Dir :=
                 Read_Default_Search_Dirs (Norm_Search_Dir, Search_File, null);

               Default_Search_Dir :=
                 new String'
                   (Concat (Norm_Search_Dir.all, Default_Suffix_Dir.all));
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

   function Include_Dir_Default_Prefix return String is
      Include_Dir : String_Access :=
                      String_Access (Update_Path (Include_Dir_Default_Name));

   begin
      if Include_Dir = null then
         return "";

      else
         declare
            Result : constant String := Include_Dir.all;
         begin
            Free (Include_Dir);
            return Result;
         end;
      end if;
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

      --  Start off by setting all suppress options to False, these will
      --  be reset later (turning some on if -gnato is not specified, and
      --  turning all of them on if -gnatp is specified).

      Suppress_Options := (others => False);

      --  Reserve the first slot in the search paths table. This is the
      --  directory of the main source file or main library file and is
      --  filled in by each call to Next_Main_Source/Next_Main_Lib_File with
      --  the directory specified for this main source or library file. This
      --  is the directory which is searched first by default. This default
      --  search is inhibited by the option -I- for both source and library
      --  files.

      Src_Search_Directories.Set_Last (Primary_Directory);
      Src_Search_Directories.Table (Primary_Directory) := new String'("");

      Lib_Search_Directories.Set_Last (Primary_Directory);
      Lib_Search_Directories.Table (Primary_Directory) := new String'("");
   end Initialize;

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      --  In addition to the default directory_separator allow the '/' to
      --  act as separator since this is allowed in MS-DOS, Windows 95/NT,
      --  and OS2 ports. On VMS, the situation is more complicated because
      --  there are two characters to check for.

      return
        C = Directory_Separator
          or else C = '/'
          or else (Hostparm.OpenVMS
                    and then (C = ']' or else C = ':'));
   end Is_Directory_Separator;

   -------------------------
   -- Is_Readonly_Library --
   -------------------------

   function Is_Readonly_Library (File : in File_Name_Type) return Boolean is
   begin
      Get_Name_String (File);

      pragma Assert (Name_Buffer (Name_Len - 3 .. Name_Len) = ".ali");

      return not Is_Writable_File (Name_Buffer (1 .. Name_Len));
   end Is_Readonly_Library;

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

   ------------------------
   -- Library_File_Stamp --
   ------------------------

   function Library_File_Stamp (N : File_Name_Type) return Time_Stamp_Type is
   begin
      return File_Stamp (Find_File (N, Library));
   end Library_File_Stamp;

   -----------------
   -- Locate_File --
   -----------------

   function Locate_File
     (N    : File_Name_Type;
      T    : File_Type;
      Dir  : Natural;
      Name : String) return File_Name_Type
   is
      Dir_Name : String_Ptr;

   begin
      --  If Name is already an absolute path, do not look for a directory

      if Is_Absolute_Path (Name) then
         Dir_Name := No_Dir;

      elsif T = Library then
         Dir_Name := Lib_Search_Directories.Table (Dir);

      else pragma Assert (T /= Config);
         Dir_Name := Src_Search_Directories.Table (Dir);
      end if;

      declare
         Full_Name : String (1 .. Dir_Name'Length + Name'Length);

      begin
         Full_Name (1 .. Dir_Name'Length) := Dir_Name.all;
         Full_Name (Dir_Name'Length + 1 .. Full_Name'Length) := Name;

         if not Is_Regular_File (Full_Name) then
            return No_File;

         else
            --  If the file is in the current directory then return N itself

            if Dir_Name'Length = 0 then
               return N;
            else
               Name_Len := Full_Name'Length;
               Name_Buffer (1 .. Name_Len) := Full_Name;
               return Name_Enter;
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
         Last_Dir  : Natural;

      begin
         if Opt.Look_In_Primary_Dir then
            File := Locate_File (N, Source, Primary_Directory, File_Name);

            if File /= No_File and then T = File_Stamp (N) then
               return File;
            end if;
         end if;

         Last_Dir := Src_Search_Directories.Last;

         for D in Primary_Directory + 1 .. Last_Dir loop
            File := Locate_File (N, Source, D, File_Name);

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
         return Lib_Search_Directories.Last -  Primary_Directory + 1;
      else
         return Lib_Search_Directories.Last -  Primary_Directory;
      end if;
   end Nb_Dir_In_Obj_Search_Path;

   -------------------------------
   -- Nb_Dir_In_Src_Search_Path --
   -------------------------------

   function Nb_Dir_In_Src_Search_Path return Natural is
   begin
      if Opt.Look_In_Primary_Dir then
         return Src_Search_Directories.Last -  Primary_Directory + 1;
      else
         return Src_Search_Directories.Last -  Primary_Directory;
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
         if File_Name (J) = Directory_Separator
           or else File_Name (J) = '/'
         then
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

         when Binder | Gnatls =>
            Dir_Name := Normalize_Directory_Name (Dir_Name.all);
            Lib_Search_Directories.Table (Primary_Directory) := Dir_Name;

         when Unspecified =>
            null;
      end case;

      Name_Len := File_Name'Last - Fptr + 1;
      Name_Buffer (1 .. Name_Len) := File_Name (Fptr .. File_Name'Last);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      Current_Main := File_Name_Type (Name_Find);

      --  In the gnatmake case, the main file may have not have the
      --  extension. Try ".adb" first then ".ads"

      if Running_Program = Make then
         declare
            Orig_Main : constant File_Name_Type := Current_Main;

         begin
            if Strip_Suffix (Orig_Main) = Orig_Main then
               Current_Main := Append_Suffix_To_File_Name (Orig_Main, ".adb");

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
         Result (1 .. Directory'Length - 1) :=
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

   function Number_Of_Files return Int is
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

   --------------------------
   -- OS_Time_To_GNAT_Time --
   --------------------------

   function OS_Time_To_GNAT_Time (T : OS_Time) return Time_Stamp_Type is
      GNAT_Time : Time_Stamp_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (T, Y, Mo, D, H, Mn, S);
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

   ------------------
   -- Program_Name --
   ------------------

   function Program_Name (Nam : String) return String_Access is
      Res : String_Access;

   begin
      --  Get the name of the current program being executed

      Find_Program_Name;

      --  Find the target prefix if any, for the cross compilation case
      --  for instance in "alpha-dec-vxworks-gcc" the target prefix is
      --  "alpha-dec-vxworks-"

      while Name_Len > 0  loop

         --  All done if we find the last hyphen

         if Name_Buffer (Name_Len) = '-' then
            exit;

         --  If directory separator found, we don't want to look further
         --  since in this case, no prefix has been found.

         elsif Is_Directory_Separator (Name_Buffer (Name_Len)) then
            Name_Len := 0;
            exit;
         end if;

         Name_Len := Name_Len - 1;
      end loop;

      --  Create the new program name

      Res := new String (1 .. Name_Len + Nam'Length);
      Res.all (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
      Res.all (Name_Len + 1 .. Name_Len + Nam'Length) := Nam;
      return Res;
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

      Buffer (1 .. Search_Dir_Prefix.all'Length)
        := Search_Dir_Prefix.all;
      Buffer (Search_Dir_Prefix.all'Length + 1 .. Buffer'Last - 1)
        := Search_File.all;
      Buffer (Buffer'Last) := ASCII.NUL;

      File_FD := Open_Read (Buffer'Address, Binary);
      if File_FD = Invalid_FD then
         return Search_Dir_Default_Name;
      end if;

      Len := Integer (File_Length (File_FD));

      --  An extra character for a trailing Path_Separator is allocated

      S := new String (1 .. Len + 1);
      S (Len + 1) := Path_Separator;

      --  Read the file. Note that the loop is not necessary since the
      --  whole file is read at once except on VMS.

      Curr := 1;
      Actual_Len := Len;
      while Actual_Len /= 0 loop
         Actual_Len := Read (File_FD, S (Curr)'Address, Len);
         Curr := Curr + Actual_Len;
      end loop;

      --  Process the file, translating line and file ending
      --  control characters to a path separator character.

      Prev_Was_Separator := True;
      Nb_Relative_Dir := 0;
      for J in 1 .. Len loop
         if S (J) in ASCII.NUL .. ASCII.US
           or else S (J) = ' '
         then
            S (J) := Path_Separator;
         end if;

         if S (J) = Path_Separator then
            Prev_Was_Separator := True;
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
      Lib_FD : File_Descriptor;
      --  The file descriptor for the current library file. A negative value
      --  indicates failure to open the specified source file.

      Text : Text_Buffer_Ptr;
      --  Allocated text buffer

      Status : Boolean;
      --  For the calls to Close

   begin
      Current_Full_Lib_Name := Find_File (Lib_File, Library);
      Current_Full_Obj_Name := Object_File_Name (Current_Full_Lib_Name);

      if Current_Full_Lib_Name = No_File then
         if Fatal_Err then
            Fail ("Cannot find: ", Name_Buffer (1 .. Name_Len));
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
            Fail ("Cannot open: ",  Name_Buffer (1 .. Name_Len));
         else
            Current_Full_Obj_Stamp := Empty_Time_Stamp;
            return null;
         end if;
      end if;

      --  Check for object file consistency if requested

      if Opt.Check_Object_Consistency then
         Current_Full_Lib_Stamp := File_Stamp (Current_Full_Lib_Name);
         Current_Full_Obj_Stamp := File_Stamp (Current_Full_Obj_Name);

         if Current_Full_Obj_Stamp (1) = ' ' then

            --  When the library is readonly, always assume that
            --  the object is consistent.

            if Is_Readonly_Library (Current_Full_Lib_Name) then
               Current_Full_Obj_Stamp := Current_Full_Lib_Stamp;

            elsif Fatal_Err then
               Get_Name_String (Current_Full_Obj_Name);
               Close (Lib_FD, Status);
               --  No need to check the status, we fail anyway

               Fail ("Cannot find: ", Name_Buffer (1 .. Name_Len));

            else
               Current_Full_Obj_Stamp := Empty_Time_Stamp;
               Close (Lib_FD, Status);

               --  No need to check the status, we return null anyway

               return null;
            end if;
         end if;
      end if;

      --  Read data from the file

      declare
         Len : constant Integer := Integer (File_Length (Lib_FD));
         --  Length of source file text. If it doesn't fit in an integer
         --  we're probably stuck anyway (>2 gigs of source seems a lot!)

         Actual_Len : Integer := 0;

         Lo : constant Text_Ptr := 0;
         --  Low bound for allocated text buffer

         Hi : Text_Ptr := Text_Ptr (Len);
         --  High bound for allocated text buffer. Note length is Len + 1
         --  which allows for extra EOF character at the end of the buffer.

      begin
         --  Allocate text buffer. Note extra character at end for EOF

         Text := new Text_Buffer (Lo .. Hi);

         --  Some systems (e.g. VMS) have file types that require one
         --  read per line, so read until we get the Len bytes or until
         --  there are no more characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Lib_FD, Text (Hi)'Address, Len);
            Hi := Hi + Text_Ptr (Actual_Len);
            exit when Actual_Len = Len or Actual_Len <= 0;
         end loop;

         Text (Hi) := EOF;
      end;

      --  Read is complete, close file and we are done

      Close (Lib_FD, Status);
      --  The status should never be False. But, if it is, what can we do?
      --  So, we don't test it.

      return Text;

   end Read_Library_Info;

   ----------------------
   -- Read_Source_File --
   ----------------------

   procedure Read_Source_File
     (N   : File_Name_Type;
      Lo  : Source_Ptr;
      Hi  : out Source_Ptr;
      Src : out Source_Buffer_Ptr;
      T   : File_Type := Source)
   is
      Source_File_FD : File_Descriptor;
      --  The file descriptor for the current source file. A negative value
      --  indicates failure to open the specified source file.

      Len : Integer;
      --  Length of file. Assume no more than 2 gigabytes of source!

      Actual_Len : Integer;

      Status : Boolean;
      --  For the call to Close

   begin
      Current_Full_Source_Name  := Find_File (N, T);
      Current_Full_Source_Stamp := File_Stamp (Current_Full_Source_Name);

      if Current_Full_Source_Name = No_File then

         --  If we were trying to access the main file and we could not
         --  find it we have an error.

         if N = Current_Main then
            Get_Name_String (N);
            Fail ("Cannot find: ", Name_Buffer (1 .. Name_Len));
         end if;

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

      Source_File_FD := Open_Read (Name_Buffer'Address, Binary);

      if Source_File_FD = Invalid_FD then
         Src := null;
         Hi  := No_Location;
         return;
      end if;

      --  Prepare to read data from the file

      Len := Integer (File_Length (Source_File_FD));

      --  Set Hi so that length is one more than the physical length,
      --  allowing for the extra EOF character at the end of the buffer

      Hi := Lo + Source_Ptr (Len);

      --  Do the actual read operation

      declare
         subtype Actual_Source_Buffer is Source_Buffer (Lo .. Hi);
         --  Physical buffer allocated

         type Actual_Source_Ptr is access Actual_Source_Buffer;
         --  This is the pointer type for the physical buffer allocated

         Actual_Ptr : constant Actual_Source_Ptr := new Actual_Source_Buffer;
         --  And this is the actual physical buffer

      begin
         --  Allocate source buffer, allowing extra character at end for EOF

         --  Some systems (e.g. VMS) have file types that require one
         --  read per line, so read until we get the Len bytes or until
         --  there are no more characters.

         Hi := Lo;
         loop
            Actual_Len := Read (Source_File_FD, Actual_Ptr (Hi)'Address, Len);
            Hi := Hi + Source_Ptr (Actual_Len);
            exit when Actual_Len = Len or Actual_Len <= 0;
         end loop;

         Actual_Ptr (Hi) := EOF;

         --  Now we need to work out the proper virtual origin pointer to
         --  return. This is exactly Actual_Ptr (0)'Address, but we have
         --  to be careful to suppress checks to compute this address.

         declare
            pragma Suppress (All_Checks);

            pragma Warnings (Off);
            --  This use of unchecked conversion is aliasing safe

            function To_Source_Buffer_Ptr is new
              Unchecked_Conversion (Address, Source_Buffer_Ptr);

            pragma Warnings (On);

         begin
            Src := To_Source_Buffer_Ptr (Actual_Ptr (0)'Address);
         end;
      end;

      --  Read is complete, get time stamp and close file and we are done

      Close (Source_File_FD, Status);

      --  The status should never be False. But, if it is, what can we do?
      --  So, we don't test it.

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
         Std_Prefix := Executable_Prefix;

         if Std_Prefix.all /= "" then

            --  Remove trailing directory separator when calling set_std_prefix

            set_std_prefix (Std_Prefix.all, Std_Prefix'Length - 1);
         end if;
      end if;

      if Path (Prefix'Range) = Prefix then
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

      if OpenVMS_On_Target then
         for K in Library'First + 2 .. Library'Last loop
            if Library (K) = '.' or else Library (K) = '-' then
               Library (K) := '_';
            end if;
         end loop;
      end if;

      return Library;
   end Shared_Lib;

   ----------------------
   -- Smart_File_Stamp --
   ----------------------

   function Smart_File_Stamp
     (N : File_Name_Type;
      T : File_Type) return Time_Stamp_Type
   is
      Time_Stamp : Time_Stamp_Type;

   begin
      if not File_Cache_Enabled then
         return File_Stamp (Find_File (N, T));
      end if;

      Time_Stamp := File_Stamp_Hash_Table.Get (N);

      if Time_Stamp (1) = ' ' then
         Time_Stamp := File_Stamp (Smart_Find_File (N, T));
         File_Stamp_Hash_Table.Set (N, Time_Stamp);
      end if;

      return Time_Stamp;
   end Smart_File_Stamp;

   ---------------------
   -- Smart_Find_File --
   ---------------------

   function Smart_Find_File
     (N : File_Name_Type;
      T : File_Type) return File_Name_Type
   is
      Full_File_Name : File_Name_Type;

   begin
      if not File_Cache_Enabled then
         return Find_File (N, T);
      end if;

      Full_File_Name := File_Name_Hash_Table.Get (N);

      if Full_File_Name = No_File then
         Full_File_Name := Find_File (N, T);
         File_Name_Hash_Table.Set (N, Full_File_Name);
      end if;

      return Full_File_Name;
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

            --  Return the part of Name that follows this last directory
            --  separator.

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
   -- To_Canonical_Dir_Spec --
   ---------------------------

   function To_Canonical_Dir_Spec
     (Host_Dir     : String;
      Prefix_Style : Boolean) return String_Access
   is
      function To_Canonical_Dir_Spec
        (Host_Dir    : Address;
         Prefix_Flag : Integer) return Address;
      pragma Import (C, To_Canonical_Dir_Spec, "__gnat_to_canonical_dir_spec");

      C_Host_Dir      : String (1 .. Host_Dir'Length + 1);
      Canonical_Dir_Addr : Address;
      Canonical_Dir_Len  : Integer;

   begin
      C_Host_Dir (1 .. Host_Dir'Length) := Host_Dir;
      C_Host_Dir (C_Host_Dir'Last)      := ASCII.NUL;

      if Prefix_Style then
         Canonical_Dir_Addr := To_Canonical_Dir_Spec (C_Host_Dir'Address, 1);
      else
         Canonical_Dir_Addr := To_Canonical_Dir_Spec (C_Host_Dir'Address, 0);
      end if;
      Canonical_Dir_Len := C_String_Length (Canonical_Dir_Addr);

      if Canonical_Dir_Len = 0 then
         return null;
      else
         return To_Path_String_Access (Canonical_Dir_Addr, Canonical_Dir_Len);
      end if;

   exception
      when others =>
         Fail ("erroneous directory spec: ", Host_Dir);
         return null;
   end To_Canonical_Dir_Spec;

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
         Canonical_File_Len  : Integer;

      begin
         --  Retrieve the expanded directoy names and build the list

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

   ----------------------------
   -- To_Canonical_File_Spec --
   ----------------------------

   function To_Canonical_File_Spec
     (Host_File : String) return String_Access
   is
      function To_Canonical_File_Spec (Host_File : Address) return Address;
      pragma Import
        (C, To_Canonical_File_Spec, "__gnat_to_canonical_file_spec");

      C_Host_File      : String (1 .. Host_File'Length + 1);
      Canonical_File_Addr : Address;
      Canonical_File_Len  : Integer;

   begin
      C_Host_File (1 .. Host_File'Length) := Host_File;
      C_Host_File (C_Host_File'Last)      := ASCII.NUL;

      Canonical_File_Addr := To_Canonical_File_Spec (C_Host_File'Address);
      Canonical_File_Len  := C_String_Length (Canonical_File_Addr);

      if Canonical_File_Len = 0 then
         return null;
      else
         return To_Path_String_Access
                  (Canonical_File_Addr, Canonical_File_Len);
      end if;

   exception
      when others =>
         Fail ("erroneous file spec: ", Host_File);
         return null;
   end To_Canonical_File_Spec;

   ----------------------------
   -- To_Canonical_Path_Spec --
   ----------------------------

   function To_Canonical_Path_Spec
     (Host_Path : String) return String_Access
   is
      function To_Canonical_Path_Spec (Host_Path : Address) return Address;
      pragma Import
        (C, To_Canonical_Path_Spec, "__gnat_to_canonical_path_spec");

      C_Host_Path         : String (1 .. Host_Path'Length + 1);
      Canonical_Path_Addr : Address;
      Canonical_Path_Len  : Integer;

   begin
      C_Host_Path (1 .. Host_Path'Length) := Host_Path;
      C_Host_Path (C_Host_Path'Last)      := ASCII.NUL;

      Canonical_Path_Addr := To_Canonical_Path_Spec (C_Host_Path'Address);
      Canonical_Path_Len  := C_String_Length (Canonical_Path_Addr);

      --  Return a null string (vice a null) for zero length paths, for
      --  compatibility with getenv().

      return To_Path_String_Access (Canonical_Path_Addr, Canonical_Path_Len);

   exception
      when others =>
         Fail ("erroneous path spec: ", Host_Path);
         return null;
   end To_Canonical_Path_Spec;

   ---------------------------
   -- To_Host_Dir_Spec --
   ---------------------------

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
      Host_Dir_Len    : Integer;

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

   ----------------------------
   -- To_Host_File_Spec --
   ----------------------------

   function To_Host_File_Spec
     (Canonical_File : String) return String_Access
   is
      function To_Host_File_Spec (Canonical_File : Address) return Address;
      pragma Import (C, To_Host_File_Spec, "__gnat_to_host_file_spec");

      C_Canonical_File      : String (1 .. Canonical_File'Length + 1);
      Host_File_Addr : Address;
      Host_File_Len  : Integer;

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
      Path_Len  : Integer) return String_Access
   is
      subtype Path_String is String (1 .. Path_Len);
      type Path_String_Access is access Path_String;

      function Address_To_Access is new
        Unchecked_Conversion (Source => Address,
                              Target => Path_String_Access);

      Path_Access : constant Path_String_Access :=
                      Address_To_Access (Path_Addr);

      Return_Val : String_Access;

   begin
      Return_Val := new String (1 .. Path_Len);

      for J in 1 .. Path_Len loop
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

      function Strlen (Str : Address) return Integer;
      pragma Import (C, Strlen, "strlen");

      procedure Strncpy (X : Address; Y : Address; Length : Integer);
      pragma Import (C, Strncpy, "strncpy");

      In_Length      : constant Integer := Path'Length;
      In_String      : String (1 .. In_Length + 1);
      Component_Name : aliased String := "GCC" & ASCII.NUL;
      Result_Ptr     : Address;
      Result_Length  : Integer;
      Out_String     : String_Ptr;

   begin
      In_String (1 .. In_Length) := Path.all;
      In_String (In_Length + 1) := ASCII.NUL;
      Result_Ptr := C_Update_Path (In_String'Address,
                                   Component_Name'Address);
      Result_Length := Strlen (Result_Ptr);

      Out_String := new String (1 .. Result_Length);
      Strncpy (Out_String.all'Address, Result_Ptr, Result_Length);
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

   procedure Write_With_Check (A  : Address; N  : Integer) is
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

   begin
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
