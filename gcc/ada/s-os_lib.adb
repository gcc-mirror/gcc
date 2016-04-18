------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        S Y S T E M . O S _ L I B                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2015, AdaCore                     --
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

pragma Compiler_Unit_Warning;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System; use System;
with System.Case_Util;
with System.CRTL;
with System.Soft_Links;

package body System.OS_Lib is

   subtype size_t is CRTL.size_t;

   procedure Strncpy (dest, src : System.Address; n : size_t)
     renames CRTL.strncpy;

   --  Imported procedures Dup and Dup2 are used in procedures Spawn and
   --  Non_Blocking_Spawn.

   function Dup (Fd : File_Descriptor) return File_Descriptor;
   pragma Import (C, Dup, "__gnat_dup");

   procedure Dup2 (Old_Fd, New_Fd : File_Descriptor);
   pragma Import (C, Dup2, "__gnat_dup2");

   function Copy_Attributes
     (From : System.Address;
      To   : System.Address;
      Mode : Integer) return Integer;
   pragma Import (C, Copy_Attributes, "__gnat_copy_attribs");
   --  Mode = 0 - copy only time stamps.
   --  Mode = 1 - copy time stamps and read/write/execute attributes
   --  Mode = 2 - copy read/write/execute attributes

   On_Windows : constant Boolean := Directory_Separator = '\';
   --  An indication that we are on Windows. Used in Normalize_Pathname, to
   --  deal with drive letters in the beginning of absolute paths.

   package SSL renames System.Soft_Links;

   --  The following are used by Create_Temp_File

   First_Temp_File_Name : constant String := "GNAT-TEMP-000000.TMP";
   --  Used to initialize Current_Temp_File_Name and Temp_File_Name_Last_Digit

   Current_Temp_File_Name : String := First_Temp_File_Name;
   --  Name of the temp file last created

   Temp_File_Name_Last_Digit : constant Positive :=
                                 First_Temp_File_Name'Last - 4;
   --  Position of the last digit in Current_Temp_File_Name

   Max_Attempts : constant := 100;
   --  The maximum number of attempts to create a new temp file

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Args_Length (Args : Argument_List) return Natural;
   --  Returns total number of characters needed to create a string of all Args
   --  terminated by ASCII.NUL characters.

   procedure Create_Temp_File_Internal
     (FD     : out File_Descriptor;
      Name   : out String_Access;
      Stdout : Boolean);
   --  Internal routine to implement two Create_Temp_File routines. If Stdout
   --  is set to True the created descriptor is stdout-compatible, otherwise
   --  it might not be depending on the OS. The first two parameters are as
   --  in Create_Temp_File.

   function C_String_Length (S : Address) return Integer;
   --  Returns the length of C (null-terminated) string at S, or 0 for
   --  Null_Address.

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Result       : out Integer;
      Pid          : out Process_Id;
      Blocking     : Boolean);
   --  Internal routine to implement the two Spawn (blocking/non blocking)
   --  routines. If Blocking is set to True then the spawn is blocking
   --  otherwise it is non blocking. In this latter case the Pid contains the
   --  process id number. The first three parameters are as in Spawn. Note that
   --  Spawn_Internal normalizes the argument list before calling the low level
   --  system spawn routines (see Normalize_Arguments).
   --
   --  Note: Normalize_Arguments is designed to do nothing if it is called more
   --  than once, so calling Normalize_Arguments before calling one of the
   --  spawn routines is fine.

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access;
   --  Converts a C String to an Ada String. We could do this making use of
   --  Interfaces.C.Strings but we prefer not to import that entire package

   ---------
   -- "<" --
   ---------

   function "<"  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) < Long_Integer (Y);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<="  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) <= Long_Integer (Y);
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">"  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) > Long_Integer (Y);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">="  (X, Y : OS_Time) return Boolean is
   begin
      return Long_Integer (X) >= Long_Integer (Y);
   end ">=";

   -----------------
   -- Args_Length --
   -----------------

   function Args_Length (Args : Argument_List) return Natural is
      Len : Natural := 0;

   begin
      for J in Args'Range loop
         Len := Len + Args (J)'Length + 1; --  One extra for ASCII.NUL
      end loop;

      return Len;
   end Args_Length;

   -----------------------------
   -- Argument_String_To_List --
   -----------------------------

   function Argument_String_To_List
     (Arg_String : String) return Argument_List_Access
   is
      Max_Args : constant Integer := Arg_String'Length;
      New_Argv : Argument_List (1 .. Max_Args);
      Idx      : Integer;
      New_Argc : Natural := 0;

      Cleaned     : String (1 .. Arg_String'Length);
      Cleaned_Idx : Natural;
      --  A cleaned up version of the argument. This function is taking
      --  backslash escapes when computing the bounds for arguments. It is
      --  then removing the extra backslashes from the argument.

      Backslash_Is_Sep : constant Boolean := Directory_Separator = '\';
      --  Whether '\' is a directory separator (as on Windows), or a way to
      --  quote special characters.

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         declare
            Backqd  : Boolean := False;
            Quoted  : Boolean := False;

         begin
            Cleaned_Idx := Cleaned'First;

            loop
               --  An unquoted space is the end of an argument

               if not (Backqd or Quoted)
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif not (Backqd or Quoted)
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;
                  Cleaned (Cleaned_Idx) := Arg_String (Idx);
                  Cleaned_Idx := Cleaned_Idx + 1;

               --  End of a quoted string and end of an argument

               elsif (Quoted and not Backqd)
                 and then Arg_String (Idx) = '"'
               then
                  Cleaned (Cleaned_Idx) := Arg_String (Idx);
                  Cleaned_Idx := Cleaned_Idx + 1;
                  Idx := Idx + 1;
                  exit;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;
                  Cleaned (Cleaned_Idx) := Arg_String (Idx);
                  Cleaned_Idx := Cleaned_Idx + 1;

               --  Following character is backquoted

               elsif not Backslash_Is_Sep and then Arg_String (Idx) = '\' then
                  Backqd := True;

               else
                  Cleaned (Cleaned_Idx) := Arg_String (Idx);
                  Cleaned_Idx := Cleaned_Idx + 1;
               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            New_Argc := New_Argc + 1;
            New_Argv (New_Argc) :=
              new String'(Cleaned (Cleaned'First .. Cleaned_Idx - 1));

            --  Skip extraneous spaces

            while Idx <= Arg_String'Last and then Arg_String (Idx) = ' ' loop
               Idx := Idx + 1;
            end loop;
         end;
      end loop;

      return new Argument_List'(New_Argv (1 .. New_Argc));
   end Argument_String_To_List;

   ---------------------
   -- C_String_Length --
   ---------------------

   function C_String_Length (S : Address) return Integer is
   begin
      if S = Null_Address then
         return 0;
      else
         return Integer (CRTL.strlen (S));
      end if;
   end C_String_Length;

   -----------
   -- Close --
   -----------

   procedure Close (FD : File_Descriptor) is
      use CRTL;
      Discard : constant int := close (int (FD));
   begin
      null;
   end Close;

   procedure Close (FD : File_Descriptor; Status : out Boolean) is
      use CRTL;
   begin
      Status := (close (int (FD)) = 0);
   end Close;

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File
     (Name     : String;
      Pathname : String;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps)
   is
      From : File_Descriptor;
      To   : File_Descriptor;

      Copy_Error : exception;
      --  Internal exception raised to signal error in copy

      function Build_Path (Dir : String; File : String) return String;
      --  Returns pathname Dir concatenated with File adding the directory
      --  separator only if needed.

      procedure Copy (From : File_Descriptor; To : File_Descriptor);
      --  Read data from From and place them into To. In both cases the
      --  operations uses the current file position. Raises Constraint_Error
      --  if a problem occurs during the copy.

      procedure Copy_To (To_Name : String);
      --  Does a straight copy from source to designated destination file

      ----------------
      -- Build_Path --
      ----------------

      function Build_Path (Dir : String; File : String) return String is
         function Is_Dirsep (C : Character) return Boolean;
         pragma Inline (Is_Dirsep);
         --  Returns True if C is a directory separator. On Windows we
         --  handle both styles of directory separator.

         ---------------
         -- Is_Dirsep --
         ---------------

         function Is_Dirsep (C : Character) return Boolean is
         begin
            return C = Directory_Separator or else C = '/';
         end Is_Dirsep;

         --  Local variables

         Base_File_Ptr : Integer;
         --  The base file name is File (Base_File_Ptr + 1 .. File'Last)

         Res : String (1 .. Dir'Length + File'Length + 1);

      --  Start of processing for Build_Path

      begin
         --  Find base file name

         Base_File_Ptr := File'Last;
         while Base_File_Ptr >= File'First loop
            exit when Is_Dirsep (File (Base_File_Ptr));
            Base_File_Ptr := Base_File_Ptr - 1;
         end loop;

         declare
            Base_File : String renames
                          File (Base_File_Ptr + 1 .. File'Last);

         begin
            Res (1 .. Dir'Length) := Dir;

            if Is_Dirsep (Dir (Dir'Last)) then
               Res (Dir'Length + 1 .. Dir'Length + Base_File'Length) :=
                 Base_File;
               return Res (1 .. Dir'Length + Base_File'Length);

            else
               Res (Dir'Length + 1) := Directory_Separator;
               Res (Dir'Length + 2 .. Dir'Length + 1 + Base_File'Length) :=
                 Base_File;
               return Res (1 .. Dir'Length + 1 + Base_File'Length);
            end if;
         end;
      end Build_Path;

      ----------
      -- Copy --
      ----------

      procedure Copy (From : File_Descriptor; To : File_Descriptor) is
         Buf_Size : constant := 200_000;
         type Buf is array (1 .. Buf_Size) of Character;
         type Buf_Ptr is access Buf;

         Buffer : Buf_Ptr;
         R      : Integer;
         W      : Integer;

         Status_From : Boolean;
         Status_To   : Boolean;
         --  Statuses for the calls to Close

         procedure Free is new Ada.Unchecked_Deallocation (Buf, Buf_Ptr);

      begin
         --  Check for invalid descriptors, making sure that we do not
         --  accidentally leave an open file descriptor around.

         if From = Invalid_FD then
            if To /= Invalid_FD then
               Close (To, Status_To);
            end if;

            raise Copy_Error;

         elsif To = Invalid_FD then
            Close (From, Status_From);
            raise Copy_Error;
         end if;

         --  Allocate the buffer on the heap

         Buffer := new Buf;

         loop
            R := Read (From, Buffer (1)'Address, Buf_Size);

            --  On some systems, the buffer may not be full. So, we need to try
            --  again until there is nothing to read.

            exit when R = 0;

            W := Write (To, Buffer (1)'Address, R);

            if W < R then

               --  Problem writing data, could be a disk full. Close files
               --  without worrying about status, since we are raising a
               --  Copy_Error exception in any case.

               Close (From, Status_From);
               Close (To, Status_To);

               Free (Buffer);

               raise Copy_Error;
            end if;
         end loop;

         Close (From, Status_From);
         Close (To, Status_To);

         Free (Buffer);

         if not (Status_From and Status_To) then
            raise Copy_Error;
         end if;
      end Copy;

      -------------
      -- Copy_To --
      -------------

      procedure Copy_To (To_Name : String) is
         C_From : String (1 .. Name'Length + 1);
         C_To   : String (1 .. To_Name'Length + 1);

      begin
         From := Open_Read (Name, Binary);

         --  Do not clobber destination file if source file could not be opened

         if From /= Invalid_FD then
            To := Create_File (To_Name, Binary);
         end if;

         Copy (From, To);

         --  Copy attributes

         C_From (1 .. Name'Length) := Name;
         C_From (C_From'Last) := ASCII.NUL;

         C_To (1 .. To_Name'Length) := To_Name;
         C_To (C_To'Last) := ASCII.NUL;

         case Preserve is
            when Time_Stamps =>
               if Copy_Attributes (C_From'Address, C_To'Address, 0) = -1 then
                  raise Copy_Error;
               end if;

            when Full =>
               if Copy_Attributes (C_From'Address, C_To'Address, 1) = -1 then
                  raise Copy_Error;
               end if;

            when None =>
               null;
         end case;

      end Copy_To;

   --  Start of processing for Copy_File

   begin
      Success := True;

      --  The source file must exist

      if not Is_Regular_File (Name) then
         raise Copy_Error;
      end if;

      --  The source file exists

      case Mode is

         --  Copy case, target file must not exist

         when Copy =>

            --  If the target file exists, we have an error

            if Is_Regular_File (Pathname) then
               raise Copy_Error;

            --  Case of target is a directory

            elsif Is_Directory (Pathname) then
               declare
                  Dest : constant String := Build_Path (Pathname, Name);

               begin
                  --  If target file exists, we have an error, else do copy

                  if Is_Regular_File (Dest) then
                     raise Copy_Error;
                  else
                     Copy_To (Dest);
                  end if;
               end;

            --  Case of normal copy to file (destination does not exist)

            else
               Copy_To (Pathname);
            end if;

         --  Overwrite case (destination file may or may not exist)

         when Overwrite =>
            if Is_Directory (Pathname) then
               Copy_To (Build_Path (Pathname, Name));
            else
               Copy_To (Pathname);
            end if;

         --  Append case (destination file may or may not exist)

         when Append =>

            --  Appending to existing file

            if Is_Regular_File (Pathname) then

               --  Append mode and destination file exists, append data at the
               --  end of Pathname. But if we fail to open source file, do not
               --  touch destination file at all.

               From := Open_Read (Name, Binary);
               if From /= Invalid_FD then
                  To := Open_Read_Write (Pathname, Binary);
               end if;

               Lseek (To, 0, Seek_End);

               Copy (From, To);

            --  Appending to directory, not allowed

            elsif Is_Directory (Pathname) then
               raise Copy_Error;

            --  Appending when target file does not exist

            else
               Copy_To (Pathname);
            end if;
      end case;

   --  All error cases are caught here

   exception
      when Copy_Error =>
         Success := False;
   end Copy_File;

   procedure Copy_File
     (Name     : C_File_Name;
      Pathname : C_File_Name;
      Success  : out Boolean;
      Mode     : Copy_Mode := Copy;
      Preserve : Attribute := Time_Stamps)
   is
      Ada_Name     : String_Access :=
                       To_Path_String_Access
                         (Name, C_String_Length (Name));
      Ada_Pathname : String_Access :=
                       To_Path_String_Access
                         (Pathname, C_String_Length (Pathname));
   begin
      Copy_File (Ada_Name.all, Ada_Pathname.all, Success, Mode, Preserve);
      Free (Ada_Name);
      Free (Ada_Pathname);
   end Copy_File;

   --------------------------
   -- Copy_File_Attributes --
   --------------------------

   procedure Copy_File_Attributes
     (From             : String;
      To               : String;
      Success          : out Boolean;
      Copy_Timestamp   : Boolean := True;
      Copy_Permissions : Boolean := True)
   is
      F    : aliased String (1 .. From'Length + 1);
      Mode : Integer;
      T    : aliased String (1 .. To'Length + 1);

   begin
      if Copy_Timestamp then
         if Copy_Permissions then
            Mode := 1;
         else
            Mode := 0;
         end if;
      else
         if Copy_Permissions then
            Mode := 2;
         else
            Success := True;
            return;  --  nothing to do
         end if;
      end if;

      F (1 .. From'Length) := From;
      F (F'Last) := ASCII.NUL;

      T (1 .. To'Length) := To;
      T (T'Last) := ASCII.NUL;

      Success := Copy_Attributes (F'Address, T'Address, Mode) /= -1;
   end Copy_File_Attributes;

   ----------------------
   -- Copy_Time_Stamps --
   ----------------------

   procedure Copy_Time_Stamps
     (Source  : String;
      Dest    : String;
      Success : out Boolean)
   is
   begin
      if Is_Regular_File (Source) and then Is_Writable_File (Dest) then
         declare
            C_Source : String (1 .. Source'Length + 1);
            C_Dest   : String (1 .. Dest'Length + 1);

         begin
            C_Source (1 .. Source'Length) := Source;
            C_Source (C_Source'Last)      := ASCII.NUL;

            C_Dest (1 .. Dest'Length) := Dest;
            C_Dest (C_Dest'Last)      := ASCII.NUL;

            if Copy_Attributes (C_Source'Address, C_Dest'Address, 0) = -1 then
               Success := False;
            else
               Success := True;
            end if;
         end;

      else
         Success := False;
      end if;
   end Copy_Time_Stamps;

   procedure Copy_Time_Stamps
     (Source  : C_File_Name;
      Dest    : C_File_Name;
      Success : out Boolean)
   is
      Ada_Source : String_Access :=
                     To_Path_String_Access
                       (Source, C_String_Length (Source));
      Ada_Dest   : String_Access :=
                     To_Path_String_Access
                       (Dest, C_String_Length (Dest));
   begin
      Copy_Time_Stamps (Ada_Source.all, Ada_Dest.all, Success);
      Free (Ada_Source);
      Free (Ada_Dest);
   end Copy_Time_Stamps;

   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Create_File
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Create_File, "__gnat_open_create");
   begin
      return C_Create_File (Name, Fmode);
   end Create_File;

   function Create_File
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Create_File (C_Name (C_Name'First)'Address, Fmode);
   end Create_File;

   ---------------------
   -- Create_New_File --
   ---------------------

   function Create_New_File
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Create_New_File
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Create_New_File, "__gnat_open_new");
   begin
      return C_Create_New_File (Name, Fmode);
   end Create_New_File;

   function Create_New_File
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Create_New_File (C_Name (C_Name'First)'Address, Fmode);
   end Create_New_File;

   -----------------------------
   -- Create_Output_Text_File --
   -----------------------------

   function Create_Output_Text_File (Name : String) return File_Descriptor is
      function C_Create_File (Name : C_File_Name) return File_Descriptor;
      pragma Import (C, C_Create_File, "__gnat_create_output_file");

      C_Name : String (1 .. Name'Length + 1);

   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return C_Create_File (C_Name (C_Name'First)'Address);
   end Create_Output_Text_File;

   ----------------------
   -- Create_Temp_File --
   ----------------------

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Temp_File_Name)
   is
      function Open_New_Temp
        (Name  : System.Address;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, Open_New_Temp, "__gnat_open_new_temp");

   begin
      FD := Open_New_Temp (Name'Address, Binary);
   end Create_Temp_File;

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out String_Access)
   is
   begin
      Create_Temp_File_Internal (FD, Name, Stdout => False);
   end Create_Temp_File;

   -----------------------------
   -- Create_Temp_Output_File --
   -----------------------------

   procedure Create_Temp_Output_File
     (FD   : out File_Descriptor;
      Name : out String_Access)
   is
   begin
      Create_Temp_File_Internal (FD, Name, Stdout => True);
   end Create_Temp_Output_File;

   -------------------------------
   -- Create_Temp_File_Internal --
   -------------------------------

   procedure Create_Temp_File_Internal
     (FD     : out File_Descriptor;
      Name   : out String_Access;
      Stdout : Boolean)
   is
      Pos      : Positive;
      Attempts : Natural := 0;
      Current  : String (Current_Temp_File_Name'Range);

      function Create_New_Output_Text_File
        (Name : String) return File_Descriptor;
      --  Similar to Create_Output_Text_File, except it fails if the file
      --  already exists. We need this behavior to ensure we don't accidentally
      --  open a temp file that has just been created by a concurrently running
      --  process. There is no point exposing this function, as it's generally
      --  not particularly useful.

      ---------------------------------
      -- Create_New_Output_Text_File --
      ---------------------------------

      function Create_New_Output_Text_File
        (Name : String) return File_Descriptor
      is
         function C_Create_File (Name : C_File_Name) return File_Descriptor;
         pragma Import (C, C_Create_File, "__gnat_create_output_file_new");

         C_Name : String (1 .. Name'Length + 1);

      begin
         C_Name (1 .. Name'Length) := Name;
         C_Name (C_Name'Last)      := ASCII.NUL;
         return C_Create_File (C_Name (C_Name'First)'Address);
      end Create_New_Output_Text_File;

   --  Start of processing for Create_Temp_File_Internal

   begin
      --  Loop until a new temp file can be created

      File_Loop : loop
         Locked : begin

            --  We need to protect global variable Current_Temp_File_Name
            --  against concurrent access by different tasks.

            SSL.Lock_Task.all;

            --  Start at the last digit

            Pos := Temp_File_Name_Last_Digit;

            Digit_Loop :
            loop
               --  Increment the digit by one

               case Current_Temp_File_Name (Pos) is
                  when '0' .. '8' =>
                     Current_Temp_File_Name (Pos) :=
                       Character'Succ (Current_Temp_File_Name (Pos));
                     exit Digit_Loop;

                  when '9' =>

                     --  For 9, set the digit to 0 and go to the previous digit

                     Current_Temp_File_Name (Pos) := '0';
                     Pos := Pos - 1;

                  when others =>

                     --  If it is not a digit, then there are no available
                     --  temp file names. Return Invalid_FD. There is almost no
                     --  chance that this code will be ever be executed, since
                     --  it would mean that there are one million temp files in
                     --  the same directory.

                     SSL.Unlock_Task.all;
                     FD := Invalid_FD;
                     Name := null;
                     exit File_Loop;
               end case;
            end loop Digit_Loop;

            Current := Current_Temp_File_Name;

            --  We can now release the lock, because we are no longer accessing
            --  Current_Temp_File_Name.

            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked;

         --  Attempt to create the file

         if Stdout then
            FD := Create_New_Output_Text_File (Current);
         else
            FD := Create_New_File (Current, Binary);
         end if;

         if FD /= Invalid_FD then
            Name := new String'(Current);
            exit File_Loop;
         end if;

         if not Is_Regular_File (Current) then

            --  If the file does not already exist and we are unable to create
            --  it, we give up after Max_Attempts. Otherwise, we try again with
            --  the next available file name.

            Attempts := Attempts + 1;

            if Attempts >= Max_Attempts then
               FD := Invalid_FD;
               Name := null;
               exit File_Loop;
            end if;
         end if;
      end loop File_Loop;
   end Create_Temp_File_Internal;

   -------------------------
   -- Current_Time_String --
   -------------------------

   function Current_Time_String return String is
      subtype S23 is String (1 .. 23);
      --  Holds current time in ISO 8601 format YYYY-MM-DD HH:MM:SS.SS + NUL

      procedure Current_Time_String (Time : System.Address);
      pragma Import (C, Current_Time_String, "__gnat_current_time_string");
      --  Puts current time into Time in above ISO 8601 format

      Result23 : aliased S23;
      --  Current time in ISO 8601 format

   begin
      Current_Time_String (Result23'Address);
      return Result23 (1 .. 19);
   end Current_Time_String;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : Address; Success : out Boolean) is
      R : Integer;
   begin
      R := System.CRTL.unlink (Name);
      Success := (R = 0);
   end Delete_File;

   procedure Delete_File (Name : String; Success : out Boolean) is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      Delete_File (C_Name'Address, Success);
   end Delete_File;

   -------------------
   -- Errno_Message --
   -------------------

   function Errno_Message
     (Err     : Integer := Errno;
      Default : String  := "") return String
   is
      function strerror (errnum : Integer) return System.Address;
      pragma Import (C, strerror, "strerror");

      C_Msg : constant System.Address := strerror (Err);

   begin
      if C_Msg = Null_Address then
         if Default /= "" then
            return Default;

         else
            --  Note: for bootstrap reasons, it is impractical
            --  to use Integer'Image here.

            declare
               Val   : Integer;
               First : Integer;

               Buf : String (1 .. 20);
               --  Buffer large enough to hold image of largest Integer values

            begin
               Val   := abs Err;
               First := Buf'Last;
               loop
                  Buf (First) :=
                    Character'Val (Character'Pos ('0') + Val mod 10);
                  Val := Val / 10;
                  exit when Val = 0;
                  First := First - 1;
               end loop;

               if Err < 0 then
                  First := First - 1;
                  Buf (First) := '-';
               end if;

               return "errno = " & Buf (First .. Buf'Last);
            end;
         end if;

      else
         declare
            Msg : String (1 .. Integer (CRTL.strlen (C_Msg)));
            for Msg'Address use C_Msg;
            pragma Import (Ada, Msg);
         begin
            return Msg;
         end;
      end if;
   end Errno_Message;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time is
      function File_Time (FD : File_Descriptor) return OS_Time;
      pragma Import (C, File_Time, "__gnat_file_time_fd");
   begin
      return File_Time (FD);
   end File_Time_Stamp;

   function File_Time_Stamp (Name : C_File_Name) return OS_Time is
      function File_Time (Name : Address) return OS_Time;
      pragma Import (C, File_Time, "__gnat_file_time_name");
   begin
      return File_Time (Name);
   end File_Time_Stamp;

   function File_Time_Stamp (Name : String) return OS_Time is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return File_Time_Stamp (F_Name'Address);
   end File_Time_Stamp;

   ---------------------------
   -- Get_Debuggable_Suffix --
   ---------------------------

   function Get_Debuggable_Suffix return String_Access is
      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "__gnat_get_debuggable_suffix_ptr");

      Result        : String_Access;
      Suffix_Length : Integer;
      Suffix_Ptr    : Address;

   begin
      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);
      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, size_t (Suffix_Length));
      end if;

      return Result;
   end Get_Debuggable_Suffix;

   ---------------------------
   -- Get_Executable_Suffix --
   ---------------------------

   function Get_Executable_Suffix return String_Access is
      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "__gnat_get_executable_suffix_ptr");

      Result        : String_Access;
      Suffix_Length : Integer;
      Suffix_Ptr    : Address;

   begin
      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);
      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, size_t (Suffix_Length));
      end if;

      return Result;
   end Get_Executable_Suffix;

   -----------------------
   -- Get_Object_Suffix --
   -----------------------

   function Get_Object_Suffix return String_Access is
      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "__gnat_get_object_suffix_ptr");

      Result        : String_Access;
      Suffix_Length : Integer;
      Suffix_Ptr    : Address;

   begin
      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);
      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, size_t (Suffix_Length));
      end if;

      return Result;
   end Get_Object_Suffix;

   ----------------------------------
   -- Get_Target_Debuggable_Suffix --
   ----------------------------------

   function Get_Target_Debuggable_Suffix return String_Access is
      Target_Exec_Ext_Ptr : Address;
      pragma Import
        (C, Target_Exec_Ext_Ptr, "__gnat_target_debuggable_extension");

      Result        : String_Access;
      Suffix_Length : Integer;

   begin
      Suffix_Length := Integer (CRTL.strlen (Target_Exec_Ext_Ptr));
      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy
           (Result.all'Address, Target_Exec_Ext_Ptr, size_t (Suffix_Length));
      end if;

      return Result;
   end Get_Target_Debuggable_Suffix;

   ----------------------------------
   -- Get_Target_Executable_Suffix --
   ----------------------------------

   function Get_Target_Executable_Suffix return String_Access is
      Target_Exec_Ext_Ptr : Address;
      pragma Import
        (C, Target_Exec_Ext_Ptr, "__gnat_target_executable_extension");

      Result        : String_Access;
      Suffix_Length : Integer;

   begin
      Suffix_Length := Integer (CRTL.strlen (Target_Exec_Ext_Ptr));
      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy
           (Result.all'Address, Target_Exec_Ext_Ptr, size_t (Suffix_Length));
      end if;

      return Result;
   end Get_Target_Executable_Suffix;

   ------------------------------
   -- Get_Target_Object_Suffix --
   ------------------------------

   function Get_Target_Object_Suffix return String_Access is
      Target_Object_Ext_Ptr : Address;
      pragma Import
        (C, Target_Object_Ext_Ptr, "__gnat_target_object_extension");

      Result        : String_Access;
      Suffix_Length : Integer;

   begin
      Suffix_Length := Integer (CRTL.strlen (Target_Object_Ext_Ptr));
      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy
           (Result.all'Address, Target_Object_Ext_Ptr, size_t (Suffix_Length));
      end if;

      return Result;
   end Get_Target_Object_Suffix;

   ------------
   -- Getenv --
   ------------

   function Getenv (Name : String) return String_Access is
      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "__gnat_getenv");

      Env_Value_Ptr    : aliased Address;
      Env_Value_Length : aliased Integer;
      F_Name           : aliased String (1 .. Name'Length + 1);
      Result           : String_Access;

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      Get_Env_Value_Ptr
        (F_Name'Address, Env_Value_Length'Address, Env_Value_Ptr'Address);

      Result := new String (1 .. Env_Value_Length);

      if Env_Value_Length > 0 then
         Strncpy
           (Result.all'Address, Env_Value_Ptr, size_t (Env_Value_Length));
      end if;

      return Result;
   end Getenv;

   ------------
   -- GM_Day --
   ------------

   function GM_Day (Date : OS_Time) return Day_Type is
      D  : Day_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Unreferenced (Y, Mo, H, Mn, S);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return D;
   end GM_Day;

   -------------
   -- GM_Hour --
   -------------

   function GM_Hour (Date : OS_Time) return Hour_Type is
      H  : Hour_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Unreferenced (Y, Mo, D, Mn, S);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return H;
   end GM_Hour;

   ---------------
   -- GM_Minute --
   ---------------

   function GM_Minute (Date : OS_Time) return Minute_Type is
      Mn : Minute_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      S  : Second_Type;
      pragma Unreferenced (Y, Mo, D, H, S);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mn;
   end GM_Minute;

   --------------
   -- GM_Month --
   --------------

   function GM_Month (Date : OS_Time) return Month_Type is
      Mo : Month_Type;

      Y  : Year_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Unreferenced (Y, D, H, Mn, S);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mo;
   end GM_Month;

   ---------------
   -- GM_Second --
   ---------------

   function GM_Second (Date : OS_Time) return Second_Type is
      S  : Second_Type;

      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      pragma Unreferenced (Y, Mo, D, H, Mn);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return S;
   end GM_Second;

   --------------
   -- GM_Split --
   --------------

   procedure GM_Split
     (Date   : OS_Time;
      Year   : out Year_Type;
      Month  : out Month_Type;
      Day    : out Day_Type;
      Hour   : out Hour_Type;
      Minute : out Minute_Type;
      Second : out Second_Type)
   is
      procedure To_GM_Time
        (P_Time_T : Address;
         P_Year   : Address;
         P_Month  : Address;
         P_Day    : Address;
         P_Hours  : Address;
         P_Mins   : Address;
         P_Secs   : Address);
      pragma Import (C, To_GM_Time, "__gnat_to_gm_time");

      T  : OS_Time := Date;
      Y  : Integer;
      Mo : Integer;
      D  : Integer;
      H  : Integer;
      Mn : Integer;
      S  : Integer;

   begin
      --  Use the global lock because To_GM_Time is not thread safe

      Locked_Processing : begin
         SSL.Lock_Task.all;
         To_GM_Time
           (P_Time_T => T'Address,
            P_Year   => Y'Address,
            P_Month  => Mo'Address,
            P_Day    => D'Address,
            P_Hours  => H'Address,
            P_Mins   => Mn'Address,
            P_Secs   => S'Address);
         SSL.Unlock_Task.all;

      exception
         when others =>
            SSL.Unlock_Task.all;
            raise;
      end Locked_Processing;

      Year   := Y + 1900;
      Month  := Mo + 1;
      Day    := D;
      Hour   := H;
      Minute := Mn;
      Second := S;
   end GM_Split;

   ----------------
   -- GM_Time_Of --
   ----------------

   function GM_Time_Of
     (Year   : Year_Type;
      Month  : Month_Type;
      Day    : Day_Type;
      Hour   : Hour_Type;
      Minute : Minute_Type;
      Second : Second_Type) return OS_Time
   is
      procedure To_OS_Time
        (P_Time_T : Address;
         P_Year   : Integer;
         P_Month  : Integer;
         P_Day    : Integer;
         P_Hours  : Integer;
         P_Mins   : Integer;
         P_Secs   : Integer);
      pragma Import (C, To_OS_Time, "__gnat_to_os_time");

      Result : OS_Time;

   begin
      To_OS_Time
        (P_Time_T => Result'Address,
         P_Year   => Year - 1900,
         P_Month  => Month - 1,
         P_Day    => Day,
         P_Hours  => Hour,
         P_Mins   => Minute,
         P_Secs   => Second);
      return Result;
   end GM_Time_Of;

   -------------
   -- GM_Year --
   -------------

   function GM_Year (Date : OS_Time) return Year_Type is
      Y  : Year_Type;

      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;
      pragma Unreferenced (Mo, D, H, Mn, S);

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Y;
   end GM_Year;

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

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Name : C_File_Name) return Boolean is
      function Is_Directory (Name : Address) return Integer;
      pragma Import (C, Is_Directory, "__gnat_is_directory");
   begin
      return Is_Directory (Name) /= 0;
   end Is_Directory;

   function Is_Directory (Name : String) return Boolean is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return Is_Directory (F_Name'Address);
   end Is_Directory;

   ----------------------
   -- Is_Readable_File --
   ----------------------

   function Is_Readable_File (Name : C_File_Name) return Boolean is
      function Is_Readable_File (Name : Address) return Integer;
      pragma Import (C, Is_Readable_File, "__gnat_is_readable_file");
   begin
      return Is_Readable_File (Name) /= 0;
   end Is_Readable_File;

   function Is_Readable_File (Name : String) return Boolean is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return Is_Readable_File (F_Name'Address);
   end Is_Readable_File;

   ------------------------
   -- Is_Executable_File --
   ------------------------

   function Is_Executable_File (Name : C_File_Name) return Boolean is
      function Is_Executable_File (Name : Address) return Integer;
      pragma Import (C, Is_Executable_File, "__gnat_is_executable_file");
   begin
      return Is_Executable_File (Name) /= 0;
   end Is_Executable_File;

   function Is_Executable_File (Name : String) return Boolean is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return Is_Executable_File (F_Name'Address);
   end Is_Executable_File;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : C_File_Name) return Boolean is
      function Is_Regular_File (Name : Address) return Integer;
      pragma Import (C, Is_Regular_File, "__gnat_is_regular_file");
   begin
      return Is_Regular_File (Name) /= 0;
   end Is_Regular_File;

   function Is_Regular_File (Name : String) return Boolean is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return Is_Regular_File (F_Name'Address);
   end Is_Regular_File;

   ----------------------
   -- Is_Symbolic_Link --
   ----------------------

   function Is_Symbolic_Link (Name : C_File_Name) return Boolean is
      function Is_Symbolic_Link (Name : Address) return Integer;
      pragma Import (C, Is_Symbolic_Link, "__gnat_is_symbolic_link");
   begin
      return Is_Symbolic_Link (Name) /= 0;
   end Is_Symbolic_Link;

   function Is_Symbolic_Link (Name : String) return Boolean is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return Is_Symbolic_Link (F_Name'Address);
   end Is_Symbolic_Link;

   ----------------------
   -- Is_Writable_File --
   ----------------------

   function Is_Writable_File (Name : C_File_Name) return Boolean is
      function Is_Writable_File (Name : Address) return Integer;
      pragma Import (C, Is_Writable_File, "__gnat_is_writable_file");
   begin
      return Is_Writable_File (Name) /= 0;
   end Is_Writable_File;

   function Is_Writable_File (Name : String) return Boolean is
      F_Name : String (1 .. Name'Length + 1);
   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;
      return Is_Writable_File (F_Name'Address);
   end Is_Writable_File;

   ----------
   -- Kill --
   ----------

   procedure Kill (Pid : Process_Id; Hard_Kill : Boolean := True) is
      SIGKILL : constant := 9;
      SIGINT  : constant := 2;

      procedure C_Kill (Pid : Process_Id; Sig_Num : Integer; Close : Integer);
      pragma Import (C, C_Kill, "__gnat_kill");

   begin
      if Hard_Kill then
         C_Kill (Pid, SIGKILL, 1);
      else
         C_Kill (Pid, SIGINT, 1);
      end if;
   end Kill;

   -----------------------
   -- Kill_Process_Tree --
   -----------------------

   procedure Kill_Process_Tree
     (Pid : Process_Id; Hard_Kill : Boolean := True)
   is
      SIGKILL : constant := 9;
      SIGINT  : constant := 2;

      procedure C_Kill_PT (Pid : Process_Id; Sig_Num : Integer);
      pragma Import (C, C_Kill_PT, "__gnat_killprocesstree");

   begin
      if Hard_Kill then
         C_Kill_PT (Pid, SIGKILL);
      else
         C_Kill_PT (Pid, SIGINT);
      end if;
   end Kill_Process_Tree;

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path
     (Exec_Name : String) return String_Access
   is
      function Locate_Exec_On_Path (C_Exec_Name : Address) return Address;
      pragma Import (C, Locate_Exec_On_Path, "__gnat_locate_exec_on_path");

      C_Exec_Name  : String (1 .. Exec_Name'Length + 1);
      Path_Addr    : Address;
      Path_Len     : Integer;
      Result       : String_Access;

   begin
      C_Exec_Name (1 .. Exec_Name'Length)   := Exec_Name;
      C_Exec_Name (C_Exec_Name'Last)        := ASCII.NUL;

      Path_Addr := Locate_Exec_On_Path (C_Exec_Name'Address);
      Path_Len  := C_String_Length (Path_Addr);

      if Path_Len = 0 then
         return null;

      else
         Result := To_Path_String_Access (Path_Addr, Path_Len);
         CRTL.free (Path_Addr);

         --  Always return an absolute path name

         if not Is_Absolute_Path (Result.all) then
            declare
               Absolute_Path : constant String :=
                 Normalize_Pathname (Result.all, Resolve_Links => False);
            begin
               Free (Result);
               Result := new String'(Absolute_Path);
            end;
         end if;

         return Result;
      end if;
   end Locate_Exec_On_Path;

   -------------------------
   -- Locate_Regular_File --
   -------------------------

   function Locate_Regular_File
     (File_Name : C_File_Name;
      Path      : C_File_Name) return String_Access
   is
      function Locate_Regular_File
        (C_File_Name, Path_Val : Address) return Address;
      pragma Import (C, Locate_Regular_File, "__gnat_locate_regular_file");

      Path_Addr    : Address;
      Path_Len     : Integer;
      Result       : String_Access;

   begin
      Path_Addr := Locate_Regular_File (File_Name, Path);
      Path_Len  := C_String_Length (Path_Addr);

      if Path_Len = 0 then
         return null;

      else
         Result := To_Path_String_Access (Path_Addr, Path_Len);
         CRTL.free (Path_Addr);
         return Result;
      end if;
   end Locate_Regular_File;

   function Locate_Regular_File
     (File_Name : String;
      Path      : String) return String_Access
   is
      C_File_Name : String (1 .. File_Name'Length + 1);
      C_Path      : String (1 .. Path'Length + 1);
      Result      : String_Access;

   begin
      C_File_Name (1 .. File_Name'Length)   := File_Name;
      C_File_Name (C_File_Name'Last)        := ASCII.NUL;

      C_Path    (1 .. Path'Length)          := Path;
      C_Path    (C_Path'Last)               := ASCII.NUL;

      Result := Locate_Regular_File (C_File_Name'Address, C_Path'Address);

      --  Always return an absolute path name

      if Result /= null and then not Is_Absolute_Path (Result.all) then
         declare
            Absolute_Path : constant String := Normalize_Pathname (Result.all);
         begin
            Free (Result);
            Result := new String'(Absolute_Path);
         end;
      end if;

      return Result;
   end Locate_Regular_File;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List) return Process_Id
   is
      Junk : Integer;
      pragma Warnings (Off, Junk);
      Pid  : Process_Id;

   begin
      Spawn_Internal (Program_Name, Args, Junk, Pid, Blocking => False);
      return Pid;
   end Non_Blocking_Spawn;

   function Non_Blocking_Spawn
     (Program_Name           : String;
      Args                   : Argument_List;
      Output_File_Descriptor : File_Descriptor;
      Err_To_Out             : Boolean := True) return Process_Id
   is
      Pid          : Process_Id;
      Saved_Error  : File_Descriptor := Invalid_FD; -- prevent warning
      Saved_Output : File_Descriptor;

   begin
      if Output_File_Descriptor = Invalid_FD then
         return Invalid_Pid;
      end if;

      --  Set standard output and, if specified, error to the temporary file

      Saved_Output := Dup (Standout);
      Dup2 (Output_File_Descriptor, Standout);

      if Err_To_Out then
         Saved_Error  := Dup (Standerr);
         Dup2 (Output_File_Descriptor, Standerr);
      end if;

      --  Spawn the program

      Pid := Non_Blocking_Spawn (Program_Name, Args);

      --  Restore the standard output and error

      Dup2 (Saved_Output, Standout);

      if Err_To_Out then
         Dup2 (Saved_Error, Standerr);
      end if;

      --  And close the saved standard output and error file descriptors

      Close (Saved_Output);

      if Err_To_Out then
         Close (Saved_Error);
      end if;

      return Pid;
   end Non_Blocking_Spawn;

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Err_To_Out   : Boolean := True) return Process_Id
   is
      Output_File_Descriptor : constant File_Descriptor :=
                                 Create_Output_Text_File (Output_File);
      Result : Process_Id;

   begin
      --  Do not attempt to spawn if the output file could not be created

      if Output_File_Descriptor = Invalid_FD then
         return Invalid_Pid;

      else
         Result :=
          Non_Blocking_Spawn
            (Program_Name, Args, Output_File_Descriptor, Err_To_Out);

         --  Close the file just created for the output, as the file descriptor
         --  cannot be used anywhere, being a local value. It is safe to do
         --  that, as the file descriptor has been duplicated to form
         --  standard output and error of the spawned process.

         Close (Output_File_Descriptor);

         return Result;
      end if;
   end Non_Blocking_Spawn;

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Stdout_File  : String;
      Stderr_File  : String) return Process_Id
   is
      Stderr_FD : constant File_Descriptor :=
                    Create_Output_Text_File (Stderr_File);
      Stdout_FD : constant File_Descriptor :=
                    Create_Output_Text_File (Stdout_File);

      Result       : Process_Id;
      Saved_Error  : File_Descriptor;
      Saved_Output : File_Descriptor;

   begin
      --  Do not attempt to spawn if the output files could not be created

      if Stdout_FD = Invalid_FD or else Stderr_FD = Invalid_FD then
         return Invalid_Pid;
      end if;

      --  Set standard output and error to the specified files

      Saved_Output := Dup (Standout);
      Dup2 (Stdout_FD, Standout);

      Saved_Error  := Dup (Standerr);
      Dup2 (Stderr_FD, Standerr);

      --  Spawn the program

      Result := Non_Blocking_Spawn (Program_Name, Args);

      --  Close the files just created for the output, as the file descriptors
      --  cannot be used anywhere, being local values. It is safe to do that,
      --  as the file descriptors have been duplicated to form standard output
      --  and standard error of the spawned process.

      Close (Stdout_FD);
      Close (Stderr_FD);

      --  Restore the standard output and error

      Dup2 (Saved_Output, Standout);
      Dup2 (Saved_Error, Standerr);

      --  And close the saved standard output and error file descriptors

      Close (Saved_Output);
      Close (Saved_Error);

      return Result;
   end Non_Blocking_Spawn;

   -------------------------
   -- Normalize_Arguments --
   -------------------------

   procedure Normalize_Arguments (Args : in out Argument_List) is
      procedure Quote_Argument (Arg : in out String_Access);
      --  Add quote around argument if it contains spaces (or HT characters)

      C_Argument_Needs_Quote : Integer;
      pragma Import (C, C_Argument_Needs_Quote, "__gnat_argument_needs_quote");
      Argument_Needs_Quote : constant Boolean := C_Argument_Needs_Quote /= 0;

      --------------------
      -- Quote_Argument --
      --------------------

      procedure Quote_Argument (Arg : in out String_Access) is
         J            : Positive := 1;
         Quote_Needed : Boolean  := False;
         Res          : String (1 .. Arg'Length * 2);

      begin
         if Arg (Arg'First) /= '"' or else Arg (Arg'Last) /= '"' then

            --  Starting quote

            Res (J) := '"';

            for K in Arg'Range loop

               J := J + 1;

               if Arg (K) = '"' then
                  Res (J) := '\';
                  J := J + 1;
                  Res (J) := '"';
                  Quote_Needed := True;

               elsif Arg (K) = ' ' or else Arg (K) = ASCII.HT then
                  Res (J) := Arg (K);
                  Quote_Needed := True;

               else
                  Res (J) := Arg (K);
               end if;
            end loop;

            if Quote_Needed then

               --  Case of null terminated string

               if Res (J) = ASCII.NUL then

                  --  If the string ends with \, double it

                  if Res (J - 1) = '\' then
                     Res (J) := '\';
                     J := J + 1;
                  end if;

                  --  Put a quote just before the null at the end

                  Res (J) := '"';
                  J := J + 1;
                  Res (J) := ASCII.NUL;

               --  If argument is terminated by '\', then double it. Otherwise
               --  the ending quote will be taken as-is. This is quite strange
               --  spawn behavior from Windows, but this is what we see.

               else
                  if Res (J) = '\' then
                     J := J + 1;
                     Res (J) := '\';
                  end if;

                  --  Ending quote

                  J := J + 1;
                  Res (J) := '"';
               end if;

               declare
                  Old : String_Access := Arg;

               begin
                  Arg := new String'(Res (1 .. J));
                  Free (Old);
               end;
            end if;

         end if;
      end Quote_Argument;

   --  Start of processing for Normalize_Arguments

   begin
      if Argument_Needs_Quote then
         for K in Args'Range loop
            if Args (K) /= null and then Args (K)'Length /= 0 then
               Quote_Argument (Args (K));
            end if;
         end loop;
      end if;
   end Normalize_Arguments;

   ------------------------
   -- Normalize_Pathname --
   ------------------------

   function Normalize_Pathname
     (Name           : String;
      Directory      : String  := "";
      Resolve_Links  : Boolean := True;
      Case_Sensitive : Boolean := True) return String
   is
      procedure Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Get_Current_Dir, "__gnat_get_current_dir");

      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");

      Max_Path : Integer;
      pragma Import (C, Max_Path, "__gnat_max_path_len");
      --  Maximum length of a path name

      function Readlink
        (Path   : System.Address;
         Buf    : System.Address;
         Bufsiz : Integer) return Integer;
      pragma Import (C, Readlink, "__gnat_readlink");

      function To_Canonical_File_Spec
        (Host_File : System.Address) return System.Address;
      pragma Import
        (C, To_Canonical_File_Spec, "__gnat_to_canonical_file_spec");
      --  Convert possible foreign file syntax to canonical form

      Fold_To_Lower_Case : constant Boolean :=
                             not Case_Sensitive
                               and then Get_File_Names_Case_Sensitive = 0;

      function Final_Value (S : String) return String;
      --  Make final adjustment to the returned string. This function strips
      --  trailing directory separators, and folds returned string to lower
      --  case if required.

      function Get_Directory  (Dir : String) return String;
      --  If Dir is not empty, return it, adding a directory separator
      --  if not already present, otherwise return current working directory
      --  with terminating directory separator.

      -----------------
      -- Final_Value --
      -----------------

      function Final_Value (S : String) return String is
         S1 : String := S;
         --  We may need to fold S to lower case, so we need a variable

         Last : Natural;

      begin
         if Fold_To_Lower_Case then
            System.Case_Util.To_Lower (S1);
         end if;

         --  Remove trailing directory separator, if any

         Last := S1'Last;

         if Last > 1
           and then (S1 (Last) = '/'
                       or else
                     S1 (Last) = Directory_Separator)
         then
            --  Special case for Windows: C:\

            if Last = 3
              and then S1 (1) /= Directory_Separator
              and then S1 (2) = ':'
            then
               null;

            else
               Last := Last - 1;
            end if;
         end if;

         return S1 (1 .. Last);
      end Final_Value;

      -------------------
      -- Get_Directory --
      -------------------

      function Get_Directory (Dir : String) return String is
      begin
         --  Directory given, add directory separator if needed

         if Dir'Length > 0 then
            declare
               Result : String   :=
                          Normalize_Pathname
                            (Dir, "", Resolve_Links, Case_Sensitive) &
                             Directory_Separator;
               Last   : Positive := Result'Last - 1;

            begin
               --  On Windows, change all '/' to '\'

               if On_Windows then
                  for J in Result'First .. Last - 1 loop
                     if Result (J) = '/' then
                        Result (J) := Directory_Separator;
                     end if;
                  end loop;
               end if;

               --  Include additional directory separator, if needed

               if Result (Last) /= Directory_Separator then
                  Last := Last + 1;
               end if;

               return Result (Result'First .. Last);
            end;

         --  Directory name not given, get current directory

         else
            declare
               Buffer   : String (1 .. Max_Path + 2);
               Path_Len : Natural := Max_Path;

            begin
               Get_Current_Dir (Buffer'Address, Path_Len'Address);

               if Buffer (Path_Len) /= Directory_Separator then
                  Path_Len := Path_Len + 1;
                  Buffer (Path_Len) := Directory_Separator;
               end if;

               --  By default, the drive letter on Windows is in upper case

               if On_Windows
                 and then Path_Len >= 2
                 and then Buffer (2) = ':'
               then
                  System.Case_Util.To_Upper (Buffer (1 .. 1));
               end if;

               return Buffer (1 .. Path_Len);
            end;
         end if;
      end Get_Directory;

      --  Local variables

      Max_Iterations : constant := 500;

      Canonical_File_Addr : System.Address;
      Canonical_File_Len  : Integer;

      End_Path    : Natural := 0;
      Finish      : Positive;
      Last        : Positive;
      Link_Buffer : String (1 .. Max_Path + 2);
      Path_Buffer : String (1 .. Max_Path + Max_Path + 2);
      Start       : Natural;
      Status      : Integer;
      The_Name    : String (1 .. Name'Length + 1);

   --  Start of processing for Normalize_Pathname

   begin
      --  Special case, return null if name is null, or if it is bigger than
      --  the biggest name allowed.

      if Name'Length = 0 or else Name'Length > Max_Path then
         return "";
      end if;

      --  First, convert possible foreign file spec to Unix file spec. If no
      --  conversion is required, all this does is put Name at the beginning
      --  of Path_Buffer unchanged.

      File_Name_Conversion : begin
         The_Name (1 .. Name'Length) := Name;
         The_Name (The_Name'Last) := ASCII.NUL;

         Canonical_File_Addr := To_Canonical_File_Spec (The_Name'Address);
         Canonical_File_Len  := Integer (CRTL.strlen (Canonical_File_Addr));

         --  If syntax conversion has failed, return an empty string to
         --  indicate the failure.

         if Canonical_File_Len = 0 then
            return "";
         end if;

         declare
            subtype Path_String is String (1 .. Canonical_File_Len);
            Canonical_File : Path_String;
            for Canonical_File'Address use Canonical_File_Addr;
            pragma Import (Ada, Canonical_File);

         begin
            Path_Buffer (1 .. Canonical_File_Len) := Canonical_File;
            End_Path := Canonical_File_Len;
            Last := 1;
         end;
      end File_Name_Conversion;

      --  Replace all '/' by Directory Separators (this is for Windows)

      if Directory_Separator /= '/' then
         for Index in 1 .. End_Path loop
            if Path_Buffer (Index) = '/' then
               Path_Buffer (Index) := Directory_Separator;
            end if;
         end loop;
      end if;

      --  Resolve directory names for Windows

      if On_Windows then

         --  On Windows, if we have an absolute path starting with a directory
         --  separator, we need to have the drive letter appended in front.

         --  On Windows, Get_Current_Dir will return a suitable directory name
         --  (path starting with a drive letter on Windows). So we take this
         --  drive letter and prepend it to the current path.

         if Path_Buffer (1) = Directory_Separator
           and then Path_Buffer (2) /= Directory_Separator
         then
            declare
               Cur_Dir : constant String := Get_Directory ("");
               --  Get the current directory to get the drive letter

            begin
               if Cur_Dir'Length > 2
                 and then Cur_Dir (Cur_Dir'First + 1) = ':'
               then
                  Path_Buffer (3 .. End_Path + 2) :=
                    Path_Buffer (1 .. End_Path);
                  Path_Buffer (1 .. 2) :=
                    Cur_Dir (Cur_Dir'First .. Cur_Dir'First + 1);
                  End_Path := End_Path + 2;
               end if;
            end;

         --  We have a drive letter, ensure it is upper-case

         elsif Path_Buffer (1) in 'a' .. 'z'
           and then Path_Buffer (2) = ':'
         then
            System.Case_Util.To_Upper (Path_Buffer (1 .. 1));
         end if;
      end if;

      --  On Windows, remove all double-quotes that are possibly part of the
      --  path but can cause problems with other methods.

      if On_Windows then
         declare
            Index : Natural;

         begin
            Index := Path_Buffer'First;
            for Current in Path_Buffer'First .. End_Path loop
               if Path_Buffer (Current) /= '"' then
                  Path_Buffer (Index) := Path_Buffer (Current);
                  Index := Index + 1;
               end if;
            end loop;

            End_Path := Index - 1;
         end;
      end if;

      --  Start the conversions

      --  If this is not finished after Max_Iterations, give up and return an
      --  empty string.

      for J in 1 .. Max_Iterations loop

         --  If we don't have an absolute pathname, prepend the directory
         --  Reference_Dir.

         if Last = 1
           and then not Is_Absolute_Path (Path_Buffer (1 .. End_Path))
         then
            declare
               Reference_Dir : constant String  := Get_Directory (Directory);
               Ref_Dir_Len   : constant Natural := Reference_Dir'Length;
               --  Current directory name specified and its length

            begin
               Path_Buffer (Ref_Dir_Len + 1 .. Ref_Dir_Len + End_Path) :=
                 Path_Buffer (1 .. End_Path);
               End_Path := Ref_Dir_Len + End_Path;
               Path_Buffer (1 .. Ref_Dir_Len) := Reference_Dir;
               Last := Ref_Dir_Len;
            end;
         end if;

         Start  := Last + 1;
         Finish := Last;

         --  Ensure that Windows network drives are kept, e.g: \\server\drive-c

         if Start = 2
           and then Directory_Separator = '\'
           and then Path_Buffer (1 .. 2) = "\\"
         then
            Start := 3;
         end if;

         --  If we have traversed the full pathname, return it

         if Start > End_Path then
            return Final_Value (Path_Buffer (1 .. End_Path));
         end if;

         --  Remove duplicate directory separators

         while Path_Buffer (Start) = Directory_Separator loop
            if Start = End_Path then
               return Final_Value (Path_Buffer (1 .. End_Path - 1));

            else
               Path_Buffer (Start .. End_Path - 1) :=
                 Path_Buffer (Start + 1 .. End_Path);
               End_Path := End_Path - 1;
            end if;
         end loop;

         --  Find the end of the current field: last character or the one
         --  preceding the next directory separator.

         while Finish < End_Path
           and then Path_Buffer (Finish + 1) /= Directory_Separator
         loop
            Finish := Finish + 1;
         end loop;

         --  Remove "." field

         if Start = Finish and then Path_Buffer (Start) = '.' then
            if Start = End_Path then
               if Last = 1 then
                  return (1 => Directory_Separator);
               else
                  if Fold_To_Lower_Case then
                     System.Case_Util.To_Lower (Path_Buffer (1 .. Last - 1));
                  end if;

                  return Path_Buffer (1 .. Last - 1);
               end if;
            else
               Path_Buffer (Last + 1 .. End_Path - 2) :=
                 Path_Buffer (Last + 3 .. End_Path);
               End_Path := End_Path - 2;
            end if;

         --  Remove ".." fields

         elsif Finish = Start + 1
           and then Path_Buffer (Start .. Finish) = ".."
         then
            Start := Last;
            loop
               Start := Start - 1;
               exit when Start < 1
                 or else Path_Buffer (Start) = Directory_Separator;
            end loop;

            if Start <= 1 then
               if Finish = End_Path then
                  return (1 => Directory_Separator);

               else
                  Path_Buffer (1 .. End_Path - Finish) :=
                    Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - Finish;
                  Last := 1;
               end if;

            else
               if Finish = End_Path then
                  return Final_Value (Path_Buffer (1 .. Start - 1));

               else
                  Path_Buffer (Start + 1 .. Start + End_Path - Finish - 1) :=
                    Path_Buffer (Finish + 2 .. End_Path);
                  End_Path := Start + End_Path - Finish - 1;
                  Last := Start;
               end if;
            end if;

         --  Check if current field is a symbolic link

         elsif Resolve_Links then
            declare
               Saved : constant Character := Path_Buffer (Finish + 1);

            begin
               Path_Buffer (Finish + 1) := ASCII.NUL;
               Status :=
                 Readlink
                   (Path   => Path_Buffer'Address,
                    Buf    => Link_Buffer'Address,
                    Bufsiz => Link_Buffer'Length);
               Path_Buffer (Finish + 1) := Saved;
            end;

            --  Not a symbolic link, move to the next field, if any

            if Status <= 0 then
               Last := Finish + 1;

            --  Replace symbolic link with its value

            else
               if Is_Absolute_Path (Link_Buffer (1 .. Status)) then
                  Path_Buffer (Status + 1 .. End_Path - (Finish - Status)) :=
                  Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - (Finish - Status);
                  Path_Buffer (1 .. Status) := Link_Buffer (1 .. Status);
                  Last := 1;

               else
                  Path_Buffer
                    (Last + Status + 1 .. End_Path - Finish + Last + Status) :=
                    Path_Buffer (Finish + 1 .. End_Path);
                  End_Path := End_Path - Finish + Last + Status;
                  Path_Buffer (Last + 1 .. Last + Status) :=
                    Link_Buffer (1 .. Status);
               end if;
            end if;

         else
            Last := Finish + 1;
         end if;
      end loop;

      --  Too many iterations: give up

      --  This can happen when there is a circularity in the symbolic links: A
      --  is a symbolic link for B, which itself is a symbolic link, and the
      --  target of B or of another symbolic link target of B is A. In this
      --  case, we return an empty string to indicate failure to resolve.

      return "";
   end Normalize_Pathname;

   -----------------
   -- Open_Append --
   -----------------

   function Open_Append
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Append
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Open_Append, "__gnat_open_append");
   begin
      return C_Open_Append (Name, Fmode);
   end Open_Append;

   function Open_Append
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Open_Append (C_Name (C_Name'First)'Address, Fmode);
   end Open_Append;

   ---------------
   -- Open_Read --
   ---------------

   function Open_Read
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Read
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Open_Read, "__gnat_open_read");
   begin
      return C_Open_Read (Name, Fmode);
   end Open_Read;

   function Open_Read
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Open_Read (C_Name (C_Name'First)'Address, Fmode);
   end Open_Read;

   ---------------------
   -- Open_Read_Write --
   ---------------------

   function Open_Read_Write
     (Name  : C_File_Name;
      Fmode : Mode) return File_Descriptor
   is
      function C_Open_Read_Write
        (Name  : C_File_Name;
         Fmode : Mode) return File_Descriptor;
      pragma Import (C, C_Open_Read_Write, "__gnat_open_rw");
   begin
      return C_Open_Read_Write (Name, Fmode);
   end Open_Read_Write;

   function Open_Read_Write
     (Name  : String;
      Fmode : Mode) return File_Descriptor
   is
      C_Name : String (1 .. Name'Length + 1);
   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;
      return Open_Read_Write (C_Name (C_Name'First)'Address, Fmode);
   end Open_Read_Write;

   -------------
   -- OS_Exit --
   -------------

   procedure OS_Exit (Status : Integer) is
   begin
      OS_Exit_Ptr (Status);
      raise Program_Error;
   end OS_Exit;

   ---------------------
   -- OS_Exit_Default --
   ---------------------

   procedure OS_Exit_Default (Status : Integer) is
      procedure GNAT_OS_Exit (Status : Integer);
      pragma Import (C, GNAT_OS_Exit, "__gnat_os_exit");
      pragma No_Return (GNAT_OS_Exit);
   begin
      GNAT_OS_Exit (Status);
   end OS_Exit_Default;

   --------------------
   -- Pid_To_Integer --
   --------------------

   function Pid_To_Integer (Pid : Process_Id) return Integer is
   begin
      return Integer (Pid);
   end Pid_To_Integer;

   ----------
   -- Read --
   ----------

   function Read
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer) return Integer
   is
   begin
      return
        Integer (System.CRTL.read
                   (System.CRTL.int (FD),
                    System.CRTL.chars (A),
                    System.CRTL.size_t (N)));
   end Read;

   -----------------
   -- Rename_File --
   -----------------

   procedure Rename_File
     (Old_Name : C_File_Name;
      New_Name : C_File_Name;
      Success  : out Boolean)
   is
      function rename (From, To : Address) return Integer;
      pragma Import (C, rename, "__gnat_rename");
      R : Integer;
   begin
      R := rename (Old_Name, New_Name);
      Success := (R = 0);
   end Rename_File;

   procedure Rename_File
     (Old_Name : String;
      New_Name : String;
      Success  : out Boolean)
   is
      C_Old_Name : String (1 .. Old_Name'Length + 1);
      C_New_Name : String (1 .. New_Name'Length + 1);
   begin
      C_Old_Name (1 .. Old_Name'Length) := Old_Name;
      C_Old_Name (C_Old_Name'Last)      := ASCII.NUL;
      C_New_Name (1 .. New_Name'Length) := New_Name;
      C_New_Name (C_New_Name'Last)      := ASCII.NUL;
      Rename_File (C_Old_Name'Address, C_New_Name'Address, Success);
   end Rename_File;

   -----------------------
   -- Set_Close_On_Exec --
   -----------------------

   procedure Set_Close_On_Exec
     (FD            : File_Descriptor;
      Close_On_Exec : Boolean;
      Status        : out Boolean)
   is
      function C_Set_Close_On_Exec
        (FD : File_Descriptor; Close_On_Exec : System.CRTL.int)
         return System.CRTL.int;
      pragma Import (C, C_Set_Close_On_Exec, "__gnat_set_close_on_exec");
   begin
      Status := C_Set_Close_On_Exec (FD, Boolean'Pos (Close_On_Exec)) = 0;
   end Set_Close_On_Exec;

   --------------------
   -- Set_Executable --
   --------------------

   procedure Set_Executable (Name : String; Mode : Positive := S_Owner) is
      procedure C_Set_Executable (Name : C_File_Name; Mode : Integer);
      pragma Import (C, C_Set_Executable, "__gnat_set_executable");
      C_Name : aliased String (Name'First .. Name'Last + 1);
   begin
      C_Name (Name'Range)  := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      C_Set_Executable (C_Name (C_Name'First)'Address, Mode);
   end Set_Executable;

   -------------------------------------
   -- Set_File_Last_Modify_Time_Stamp --
   -------------------------------------

   procedure Set_File_Last_Modify_Time_Stamp (Name : String; Time : OS_Time) is
      procedure C_Set_File_Time (Name : C_File_Name; Time : OS_Time);
      pragma Import (C, C_Set_File_Time, "__gnat_set_file_time_name");
      C_Name : aliased String (Name'First .. Name'Last + 1);
   begin
      C_Name (Name'Range)  := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      C_Set_File_Time (C_Name'Address, Time);
   end Set_File_Last_Modify_Time_Stamp;

   ----------------------
   -- Set_Non_Readable --
   ----------------------

   procedure Set_Non_Readable (Name : String) is
      procedure C_Set_Non_Readable (Name : C_File_Name);
      pragma Import (C, C_Set_Non_Readable, "__gnat_set_non_readable");
      C_Name : aliased String (Name'First .. Name'Last + 1);
   begin
      C_Name (Name'Range)  := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      C_Set_Non_Readable (C_Name (C_Name'First)'Address);
   end Set_Non_Readable;

   ----------------------
   -- Set_Non_Writable --
   ----------------------

   procedure Set_Non_Writable (Name : String) is
      procedure C_Set_Non_Writable (Name : C_File_Name);
      pragma Import (C, C_Set_Non_Writable, "__gnat_set_non_writable");
      C_Name : aliased String (Name'First .. Name'Last + 1);
   begin
      C_Name (Name'Range)  := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      C_Set_Non_Writable (C_Name (C_Name'First)'Address);
   end Set_Non_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable (Name : String) is
      procedure C_Set_Readable (Name : C_File_Name);
      pragma Import (C, C_Set_Readable, "__gnat_set_readable");
      C_Name : aliased String (Name'First .. Name'Last + 1);
   begin
      C_Name (Name'Range)  := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      C_Set_Readable (C_Name (C_Name'First)'Address);
   end Set_Readable;

   --------------------
   -- Set_Writable --
   --------------------

   procedure Set_Writable (Name : String) is
      procedure C_Set_Writable (Name : C_File_Name);
      pragma Import (C, C_Set_Writable, "__gnat_set_writable");
      C_Name : aliased String (Name'First .. Name'Last + 1);
   begin
      C_Name (Name'Range)  := Name;
      C_Name (C_Name'Last) := ASCII.NUL;
      C_Set_Writable (C_Name (C_Name'First)'Address);
   end Set_Writable;

   ------------
   -- Setenv --
   ------------

   procedure Setenv (Name : String; Value : String) is
      F_Name  : String (1 .. Name'Length + 1);
      F_Value : String (1 .. Value'Length + 1);

      procedure Set_Env_Value (Name, Value : System.Address);
      pragma Import (C, Set_Env_Value, "__gnat_setenv");

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      F_Value (1 .. Value'Length) := Value;
      F_Value (F_Value'Last)      := ASCII.NUL;

      Set_Env_Value (F_Name'Address, F_Value'Address);
   end Setenv;

   -----------
   -- Spawn --
   -----------

   function Spawn
     (Program_Name : String;
      Args         : Argument_List) return Integer
   is
      Junk   : Process_Id;
      pragma Warnings (Off, Junk);
      Result : Integer;

   begin
      Spawn_Internal (Program_Name, Args, Result, Junk, Blocking => True);
      return Result;
   end Spawn;

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean)
   is
   begin
      Success := (Spawn (Program_Name, Args) = 0);
   end Spawn;

   procedure Spawn
     (Program_Name           : String;
      Args                   : Argument_List;
      Output_File_Descriptor : File_Descriptor;
      Return_Code            : out Integer;
      Err_To_Out             : Boolean := True)
   is
      Saved_Error  : File_Descriptor := Invalid_FD; -- prevent compiler warning
      Saved_Output : File_Descriptor;

   begin
      --  Set standard output and error to the temporary file

      Saved_Output := Dup (Standout);
      Dup2 (Output_File_Descriptor, Standout);

      if Err_To_Out then
         Saved_Error  := Dup (Standerr);
         Dup2 (Output_File_Descriptor, Standerr);
      end if;

      --  Spawn the program

      Return_Code := Spawn (Program_Name, Args);

      --  Restore the standard output and error

      Dup2 (Saved_Output, Standout);

      if Err_To_Out then
         Dup2 (Saved_Error, Standerr);
      end if;

      --  And close the saved standard output and error file descriptors

      Close (Saved_Output);

      if Err_To_Out then
         Close (Saved_Error);
      end if;
   end Spawn;

   procedure Spawn
     (Program_Name : String;
      Args         : Argument_List;
      Output_File  : String;
      Success      : out Boolean;
      Return_Code  : out Integer;
      Err_To_Out   : Boolean := True)
   is
      FD : File_Descriptor;

   begin
      Success := True;
      Return_Code := 0;

      FD := Create_Output_Text_File (Output_File);

      if FD = Invalid_FD then
         Success := False;
         return;
      end if;

      Spawn (Program_Name, Args, FD, Return_Code, Err_To_Out);

      Close (FD, Success);
   end Spawn;

   --------------------
   -- Spawn_Internal --
   --------------------

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Result       : out Integer;
      Pid          : out Process_Id;
      Blocking     : Boolean)
   is
      procedure Spawn (Args : Argument_List);
      --  Call Spawn with given argument list

      N_Args : Argument_List (Args'Range);
      --  Normalized arguments

      -----------
      -- Spawn --
      -----------

      procedure Spawn (Args : Argument_List) is
         type Chars is array (Positive range <>) of aliased Character;
         type Char_Ptr is access constant Character;

         Command_Len  : constant Positive := Program_Name'Length + 1 +
                                               Args_Length (Args);
         Command_Last : Natural := 0;
         Command      : aliased Chars (1 .. Command_Len);
         --  Command contains all characters of the Program_Name and Args, all
         --  terminated by ASCII.NUL characters.

         Arg_List_Len  : constant Positive := Args'Length + 2;
         Arg_List_Last : Natural := 0;
         Arg_List      : aliased array (1 .. Arg_List_Len) of Char_Ptr;
         --  List with pointers to NUL-terminated strings of the Program_Name
         --  and the Args and terminated with a null pointer. We rely on the
         --  default initialization for the last null pointer.

         procedure Add_To_Command (S : String);
         --  Add S and a NUL character to Command, updating Last

         function Portable_Spawn (Args : Address) return Integer;
         pragma Import (C, Portable_Spawn, "__gnat_portable_spawn");

         function Portable_No_Block_Spawn (Args : Address) return Process_Id;
         pragma Import
           (C, Portable_No_Block_Spawn, "__gnat_portable_no_block_spawn");

         --------------------
         -- Add_To_Command --
         --------------------

         procedure Add_To_Command (S : String) is
            First : constant Natural := Command_Last + 1;

         begin
            Command_Last := Command_Last + S'Length;

            --  Move characters one at a time, because Command has aliased
            --  components.

            --  But not volatile, so why is this necessary ???

            for J in S'Range loop
               Command (First + J - S'First) := S (J);
            end loop;

            Command_Last := Command_Last + 1;
            Command (Command_Last) := ASCII.NUL;

            Arg_List_Last := Arg_List_Last + 1;
            Arg_List (Arg_List_Last) := Command (First)'Access;
         end Add_To_Command;

      --  Start of processing for Spawn

      begin
         Add_To_Command (Program_Name);

         for J in Args'Range loop
            Add_To_Command (Args (J).all);
         end loop;

         if Blocking then
            Pid    := Invalid_Pid;
            Result := Portable_Spawn (Arg_List'Address);
         else
            Pid    := Portable_No_Block_Spawn (Arg_List'Address);
            Result := Boolean'Pos (Pid /= Invalid_Pid);
         end if;
      end Spawn;

   --  Start of processing for Spawn_Internal

   begin
      --  Copy arguments into a local structure

      for K in N_Args'Range loop
         N_Args (K) := new String'(Args (K).all);
      end loop;

      --  Normalize those arguments

      Normalize_Arguments (N_Args);

      --  Call spawn using the normalized arguments

      Spawn (N_Args);

      --  Free arguments list

      for K in N_Args'Range loop
         Free (N_Args (K));
      end loop;
   end Spawn_Internal;

   ---------------------------
   -- To_Path_String_Access --
   ---------------------------

   function To_Path_String_Access
     (Path_Addr : Address;
      Path_Len  : Integer) return String_Access
   is
      subtype Path_String is String (1 .. Path_Len);
      type    Path_String_Access is access Path_String;

      function Address_To_Access is new Ada.Unchecked_Conversion
        (Source => Address, Target => Path_String_Access);

      Path_Access : constant Path_String_Access :=
                      Address_To_Access (Path_Addr);

      Return_Val  : String_Access;

   begin
      Return_Val := new String (1 .. Path_Len);

      for J in 1 .. Path_Len loop
         Return_Val (J) := Path_Access (J);
      end loop;

      return Return_Val;
   end To_Path_String_Access;

   ------------------
   -- Wait_Process --
   ------------------

   procedure Wait_Process (Pid : out Process_Id; Success : out Boolean) is
      Status : Integer;

      function Portable_Wait (S : Address) return Process_Id;
      pragma Import (C, Portable_Wait, "__gnat_portable_wait");

   begin
      Pid := Portable_Wait (Status'Address);
      Success := (Status = 0);
   end Wait_Process;

   -----------
   -- Write --
   -----------

   function Write
     (FD : File_Descriptor;
      A  : System.Address;
      N  : Integer) return Integer
   is
   begin
      return
        Integer (System.CRTL.write
                   (System.CRTL.int (FD),
                    System.CRTL.chars (A),
                    System.CRTL.size_t (N)));
   end Write;

end System.OS_Lib;
