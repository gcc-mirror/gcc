------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G N A T . O S _ L I B                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1995-2003 Ada Core Technologies, Inc.            --
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

with System.Case_Util;
with System.CRTL;
with System.Soft_Links;
with Unchecked_Conversion;
with System; use System;

package body GNAT.OS_Lib is

   package SSL renames System.Soft_Links;

   --  The following are used by Create_Temp_File

   Current_Temp_File_Name : String := "GNAT-TEMP-000000.TMP";
   --  Name of the temp file last created

   Temp_File_Name_Last_Digit : constant Positive :=
                                 Current_Temp_File_Name'Last - 4;
   --  Position of the last digit in Current_Temp_File_Name

   Max_Attempts : constant := 100;
   --  The maximum number of attempts to create a new temp file

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Args_Length (Args : Argument_List) return Natural;
   --  Returns total number of characters needed to create a string
   --  of all Args terminated by ASCII.NUL characters

   function C_String_Length (S : Address) return Integer;
   --  Returns the length of a C string. Does check for null address
   --  (returns 0).

   procedure Spawn_Internal
     (Program_Name : String;
      Args         : Argument_List;
      Result       : out Integer;
      Pid          : out Process_Id;
      Blocking     : Boolean);
   --  Internal routine to implement the two Spawn (blocking/non blocking)
   --  routines. If Blocking is set to True then the spawn is blocking
   --  otherwise it is non blocking. In this latter case the Pid contains
   --  the process id number. The first three parameters are as in Spawn.
   --  Note that Spawn_Internal normalizes the argument list before calling
   --  the low level system spawn routines (see Normalize_Arguments). Note
   --  that Normalize_Arguments is designed to do nothing if it is called
   --  more than once, so calling Normalize_Arguments before calling one
   --  of the spawn routines is fine.

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
      New_Argc : Natural := 0;
      Idx      : Integer;

   begin
      Idx := Arg_String'First;

      loop
         exit when Idx > Arg_String'Last;

         declare
            Quoted  : Boolean := False;
            Backqd  : Boolean := False;
            Old_Idx : Integer;

         begin
            Old_Idx := Idx;

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

               --  End of a quoted string and end of an argument

               elsif (Quoted and not Backqd)
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\' then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            New_Argc := New_Argc + 1;
            New_Argv (New_Argc) :=
              new String'(Arg_String (Old_Idx .. Idx - 1));

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

      function Strlen (S : Address) return Integer;
      pragma Import (C, Strlen, "strlen");

   begin
      if S = Null_Address then
         return 0;
      else
         return Strlen (S);
      end if;
   end C_String_Length;

   -----------
   -- Close --
   -----------

   procedure Close (FD : File_Descriptor) is
      procedure C_Close (FD : File_Descriptor);
      pragma Import (C, C_Close, "close");
   begin
      C_Close (FD);
   end Close;

   procedure Close (FD : File_Descriptor; Status : out Boolean) is
      function C_Close (FD : File_Descriptor) return Integer;
      pragma Import (C, C_Close, "close");
   begin
      Status := (C_Close (FD) = 0);
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
      --  Returns pathname Dir catenated with File adding the directory
      --  separator only if needed.

      procedure Copy (From, To : File_Descriptor);
      --  Read data from From and place them into To. In both cases the
      --  operations uses the current file position. Raises Constraint_Error
      --  if a problem occurs during the copy.

      procedure Copy_To (To_Name : String);
      --  Does a straight copy from source to designated destination file

      ----------------
      -- Build_Path --
      ----------------

      function Build_Path (Dir : String; File : String) return String is
         Res : String (1 .. Dir'Length + File'Length + 1);

         Base_File_Ptr : Integer;
         --  The base file name is File (Base_File_Ptr + 1 .. File'Last)

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

      procedure Copy (From, To : File_Descriptor) is
         Buf_Size : constant := 200_000;
         Buffer   : array (1 .. Buf_Size) of Character;
         R        : Integer;
         W        : Integer;

         Status_From : Boolean;
         Status_To   : Boolean;
         --  Statuses for the calls to Close

      begin
         if From = Invalid_FD or else To = Invalid_FD then
            raise Copy_Error;
         end if;

         loop
            R := Read (From, Buffer (1)'Address, Buf_Size);

            --  For VMS, the buffer may not be full. So, we need to try again
            --  until there is nothing to read.

            exit when R = 0;

            W := Write (To, Buffer (1)'Address, R);

            if W < R then

               --  Problem writing data, could be a disk full. Close files
               --  without worrying about status, since we are raising a
               --  Copy_Error exception in any case.

               Close (From, Status_From);
               Close (To, Status_To);

               raise Copy_Error;
            end if;
         end loop;

         Close (From, Status_From);
         Close (To, Status_To);

         if not (Status_From and Status_To) then
            raise Copy_Error;
         end if;
      end Copy;

      -------------
      -- Copy_To --
      -------------

      procedure Copy_To (To_Name : String) is

         function Copy_Attributes
           (From, To : System.Address;
            Mode     : Integer) return Integer;
         pragma Import (C, Copy_Attributes, "__gnat_copy_attribs");
         --  Mode = 0 - copy only time stamps.
         --  Mode = 1 - copy time stamps and read/write/execute attributes

         C_From : String (1 .. Name'Length + 1);
         C_To   : String (1 .. To_Name'Length + 1);

      begin
         From := Open_Read (Name, Binary);
         To   := Create_File (To_Name, Binary);
         Copy (From, To);

         --  Copy attributes

         C_From (1 .. Name'Length) := Name;
         C_From (C_From'Last) := ASCII.Nul;

         C_To (1 .. To_Name'Length) := To_Name;
         C_To (C_To'Last) := ASCII.Nul;

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
                  --  If the target file exists, we have an error
                  --  otherwise do the copy.

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

         --  Overwrite case, destination file may or may not exist

         when Overwrite =>
            if Is_Directory (Pathname) then
               Copy_To (Build_Path (Pathname, Name));
            else
               Copy_To (Pathname);
            end if;

         --  Appending case, destination file may or may not exist

         when Append =>

            --  Appending to existing file

            if Is_Regular_File (Pathname) then

               --  Append mode and destination file exists, append data
               --  at the end of Pathname.

               From := Open_Read (Name, Binary);
               To   := Open_Read_Write (Pathname, Binary);
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
      Ada_Name : String_Access :=
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

   ----------------------
   -- Copy_Time_Stamps --
   ----------------------

   procedure Copy_Time_Stamps (Source, Dest : String; Success : out Boolean) is

      function Copy_Attributes
        (From, To : System.Address;
         Mode     : Integer) return Integer;
      pragma Import (C, Copy_Attributes, "__gnat_copy_attribs");
      --  Mode = 0 - copy only time stamps.
      --  Mode = 1 - copy time stamps and read/write/execute attributes

   begin
      if Is_Regular_File (Source) and then Is_Writable_File (Dest) then
         declare
            C_Source : String (1 .. Source'Length + 1);
            C_Dest   : String (1 .. Dest'Length + 1);
         begin
            C_Source (1 .. C_Source'Length) := Source;
            C_Source (C_Source'Last)        := ASCII.Nul;

            C_Dest (1 .. C_Dest'Length) := Dest;
            C_Dest (C_Dest'Last)        := ASCII.Nul;

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
     (Source, Dest : C_File_Name;
      Success      : out Boolean)
   is
      Ada_Source : String_Access :=
                     To_Path_String_Access
                       (Source, C_String_Length (Source));

      Ada_Dest : String_Access :=
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
      Pos      : Positive;
      Attempts : Natural := 0;
      Current  : String (Current_Temp_File_Name'Range);

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
                     --  temp file names. Return Invalid_FD. There is almost
                     --  no that this code will be ever be executed, since
                     --  it would mean that there are one million temp files
                     --  in the same directory!

                     SSL.Unlock_Task.all;
                     FD := Invalid_FD;
                     Name := null;
                     exit File_Loop;
               end case;
            end loop Digit_Loop;

            Current := Current_Temp_File_Name;

            --  We can now release the lock, because we are no longer
            --  accessing Current_Temp_File_Name.

            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked;

         --  Attempt to create the file

         FD := Create_New_File (Current, Binary);

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
   end Create_Temp_File;

   -----------------
   -- Delete_File --
   -----------------

   procedure Delete_File (Name : Address; Success : out Boolean) is
      R : Integer;

      function unlink (A : Address) return Integer;
      pragma Import (C, unlink, "unlink");

   begin
      R := unlink (Name);
      Success := (R = 0);
   end Delete_File;

   procedure Delete_File (Name : String; Success : out Boolean) is
      C_Name : String (1 .. Name'Length + 1);

   begin
      C_Name (1 .. Name'Length) := Name;
      C_Name (C_Name'Last)      := ASCII.NUL;

      Delete_File (C_Name'Address, Success);
   end Delete_File;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (FD : File_Descriptor) return OS_Time is
      function File_Time (FD    : File_Descriptor) return OS_Time;
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

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Suffix_Ptr    : Address;
      Suffix_Length : Integer;
      Result        : String_Access;

   begin
      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);

      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, Suffix_Length);
      end if;

      return Result;
   end Get_Debuggable_Suffix;

   ---------------------------
   -- Get_Executable_Suffix --
   ---------------------------

   function Get_Executable_Suffix return String_Access is
      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "__gnat_get_executable_suffix_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Suffix_Ptr    : Address;
      Suffix_Length : Integer;
      Result        : String_Access;

   begin
      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);

      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, Suffix_Length);
      end if;

      return Result;
   end Get_Executable_Suffix;

   -----------------------
   -- Get_Object_Suffix --
   -----------------------

   function Get_Object_Suffix return String_Access is
      procedure Get_Suffix_Ptr (Length, Ptr : Address);
      pragma Import (C, Get_Suffix_Ptr, "__gnat_get_object_suffix_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

      Suffix_Ptr    : Address;
      Suffix_Length : Integer;
      Result        : String_Access;

   begin
      Get_Suffix_Ptr (Suffix_Length'Address, Suffix_Ptr'Address);

      Result := new String (1 .. Suffix_Length);

      if Suffix_Length > 0 then
         Strncpy (Result.all'Address, Suffix_Ptr, Suffix_Length);
      end if;

      return Result;
   end Get_Object_Suffix;

   ------------
   -- Getenv --
   ------------

   function Getenv (Name : String) return String_Access is
      procedure Get_Env_Value_Ptr (Name, Length, Ptr : Address);
      pragma Import (C, Get_Env_Value_Ptr, "__gnat_get_env_value_ptr");

      procedure Strncpy (Astring_Addr, Cstring : Address; N : Integer);
      pragma Import (C, Strncpy, "strncpy");

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
         Strncpy (Result.all'Address, Env_Value_Ptr, Env_Value_Length);
      end if;

      return Result;
   end Getenv;

   ------------
   -- GM_Day --
   ------------

   function GM_Day (Date : OS_Time) return Day_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return D;
   end GM_Day;

   -------------
   -- GM_Hour --
   -------------

   function GM_Hour (Date : OS_Time) return Hour_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return H;
   end GM_Hour;

   ---------------
   -- GM_Minute --
   ---------------

   function GM_Minute (Date : OS_Time) return Minute_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mn;
   end GM_Minute;

   --------------
   -- GM_Month --
   --------------

   function GM_Month (Date : OS_Time) return Month_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Mo;
   end GM_Month;

   ---------------
   -- GM_Second --
   ---------------

   function GM_Second (Date : OS_Time) return Second_Type is
      Y  : Year_Type;
      Mo : Month_Type;
      D  : Day_Type;
      H  : Hour_Type;
      Mn : Minute_Type;
      S  : Second_Type;

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
        (P_Time_T, P_Year, P_Month, P_Day, P_Hours, P_Mins, P_Secs : Address);
      pragma Import (C, To_GM_Time, "__gnat_to_gm_time");

      T  : OS_Time := Date;
      Y  : Integer;
      Mo : Integer;
      D  : Integer;
      H  : Integer;
      Mn : Integer;
      S  : Integer;

   begin
      --  Use the global lock because To_GM_Time is not thread safe.

      Locked_Processing : begin
         SSL.Lock_Task.all;
         To_GM_Time
           (T'Address, Y'Address, Mo'Address, D'Address,
            H'Address, Mn'Address, S'Address);
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

   begin
      GM_Split (Date, Y, Mo, D, H, Mn, S);
      return Y;
   end GM_Year;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (Name : String) return Boolean is
      function Is_Absolute_Path (Name : Address) return Integer;
      pragma Import (C, Is_Absolute_Path, "__gnat_is_absolute_path");

      F_Name : String (1 .. Name'Length + 1);

   begin
      F_Name (1 .. Name'Length) := Name;
      F_Name (F_Name'Last)      := ASCII.NUL;

      return Is_Absolute_Path (F_Name'Address) /= 0;
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

   -------------------------
   -- Locate_Exec_On_Path --
   -------------------------

   function Locate_Exec_On_Path
     (Exec_Name : String) return String_Access
   is
      function Locate_Exec_On_Path (C_Exec_Name : Address) return Address;
      pragma Import (C, Locate_Exec_On_Path, "__gnat_locate_exec_on_path");

      procedure Free (Ptr : System.Address);
      pragma Import (C, Free, "free");

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
         Free (Path_Addr);
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

      procedure Free (Ptr : System.Address);
      pragma Import (C, Free, "free");

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
         Free (Path_Addr);
         return Result;
      end if;
   end Locate_Regular_File;

   function Locate_Regular_File
     (File_Name : String;
      Path      : String) return String_Access
   is
      C_File_Name : String (1 .. File_Name'Length + 1);
      C_Path      : String (1 .. Path'Length + 1);

   begin
      C_File_Name (1 .. File_Name'Length)   := File_Name;
      C_File_Name (C_File_Name'Last)        := ASCII.NUL;

      C_Path    (1 .. Path'Length)          := Path;
      C_Path    (C_Path'Last)               := ASCII.NUL;

      return Locate_Regular_File (C_File_Name'Address, C_Path'Address);
   end Locate_Regular_File;

   ------------------------
   -- Non_Blocking_Spawn --
   ------------------------

   function Non_Blocking_Spawn
     (Program_Name : String;
      Args         : Argument_List) return Process_Id
   is
      Junk : Integer;
      Pid  : Process_Id;

   begin
      Spawn_Internal (Program_Name, Args, Junk, Pid, Blocking => False);
      return Pid;
   end Non_Blocking_Spawn;

   -------------------------
   -- Normalize_Arguments --
   -------------------------

   procedure Normalize_Arguments (Args : in out Argument_List) is

      procedure Quote_Argument (Arg : in out String_Access);
      --  Add quote around argument if it contains spaces

      C_Argument_Needs_Quote : Integer;
      pragma Import (C, C_Argument_Needs_Quote, "__gnat_argument_needs_quote");
      Argument_Needs_Quote : constant Boolean := C_Argument_Needs_Quote /= 0;

      --------------------
      -- Quote_Argument --
      --------------------

      procedure Quote_Argument (Arg : in out String_Access) is
         Res          : String (1 .. Arg'Length * 2);
         J            : Positive := 1;
         Quote_Needed : Boolean  := False;

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

               elsif Arg (K) = ' ' then
                  Res (J) := Arg (K);
                  Quote_Needed := True;

               else
                  Res (J) := Arg (K);
               end if;

            end loop;

            if Quote_Needed then

               --  If null terminated string, put the quote before

               if Res (J) = ASCII.Nul then
                  Res (J) := '"';
                  J := J + 1;
                  Res (J) := ASCII.Nul;

               --  If argument is terminated by '\', then double it. Otherwise
               --  the ending quote will be taken as-is. This is quite strange
               --  spawn behavior from Windows, but this is what we see!

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
      Max_Path : Integer;
      pragma Import (C, Max_Path, "__gnat_max_path_len");
      --  Maximum length of a path name

      procedure Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Get_Current_Dir, "__gnat_get_current_dir");

      function Change_Dir (Dir_Name : String) return Integer;
      pragma Import (C, Change_Dir, "chdir");

      Path_Buffer : String (1 .. Max_Path + Max_Path + 2);
      End_Path    : Natural := 0;
      Link_Buffer : String (1 .. Max_Path + 2);
      Status      : Integer;
      Last        : Positive;
      Start       : Natural;
      Finish      : Positive;

      Max_Iterations : constant := 500;

      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");

      Fold_To_Lower_Case : constant Boolean :=
                             not Case_Sensitive
                               and then Get_File_Names_Case_Sensitive = 0;

      function Readlink
        (Path   : System.Address;
         Buf    : System.Address;
         Bufsiz : Integer) return Integer;
      pragma Import (C, Readlink, "__gnat_readlink");

      function To_Canonical_File_Spec
        (Host_File : System.Address) return System.Address;
      pragma Import
        (C, To_Canonical_File_Spec, "__gnat_to_canonical_file_spec");

      The_Name : String (1 .. Name'Length + 1);
      Canonical_File_Addr : System.Address;
      Canonical_File_Len  : Integer;

      Need_To_Check_Drive_Letter : Boolean := False;
      --  Set to true if Name is an absolute path that starts with "//"

      function Strlen (S : System.Address) return Integer;
      pragma Import (C, Strlen, "strlen");

      function Get_Directory  (Dir : String) return String;
      --  If Dir is not empty, return it, adding a directory separator
      --  if not already present, otherwise return current working directory
      --  with terminating directory separator.

      function Final_Value (S : String) return String;
      --  Make final adjustment to the returned string.
      --  To compensate for non standard path name in Interix,
      --  if S is "/x" or starts with "/x", where x is a capital
      --  letter 'A' to 'Z', add an additional '/' at the beginning
      --  so that the returned value starts with "//x".

      -------------------
      -- Get_Directory --
      -------------------

      function Get_Directory (Dir : String) return String is
      begin
         --  Directory given, add directory separator if needed

         if Dir'Length > 0 then
            if Dir (Dir'Length) = Directory_Separator then
               return Directory;
            else
               declare
                  Result : String (1 .. Dir'Length + 1);

               begin
                  Result (1 .. Dir'Length) := Dir;
                  Result (Result'Length) := Directory_Separator;
                  return Result;
               end;
            end if;

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

               return Buffer (1 .. Path_Len);
            end;
         end if;
      end Get_Directory;

      Reference_Dir : constant String := Get_Directory (Directory);
      --  Current directory name specified

      -----------------
      -- Final_Value --
      -----------------

      function Final_Value (S : String) return String is
         S1 : String := S;
         --  We may need to fold S to lower case, so we need a variable

      begin
         --  Interix has the non standard notion of disk drive
         --  indicated by two '/' followed by a capital letter
         --  'A' .. 'Z'. One of the two '/' may have been removed
         --  by Normalize_Pathname. It has to be added again.
         --  For other OSes, this should not make no difference.

         if Need_To_Check_Drive_Letter
           and then S'Length >= 2
           and then S (S'First) = '/'
           and then S (S'First + 1) in 'A' .. 'Z'
           and then (S'Length = 2 or else S (S'First + 2) = '/')
         then
            declare
               Result : String (1 .. S'Length + 1);

            begin
               Result (1) := '/';
               Result (2 .. Result'Last) := S;

               if Fold_To_Lower_Case then
                  System.Case_Util.To_Lower (Result);
               end if;

               return Result;

            end;

         else

            if Fold_To_Lower_Case then
               System.Case_Util.To_Lower (S1);
            end if;

            return S1;

         end if;

      end Final_Value;

   --  Start of processing for Normalize_Pathname

   begin
      --  Special case, if name is null, then return null

      if Name'Length = 0 then
         return "";
      end if;

      --  First, convert VMS file spec to Unix file spec.
      --  If Name is not in VMS syntax, then this is equivalent
      --  to put Name at the begining of Path_Buffer.

      VMS_Conversion : begin
         The_Name (1 .. Name'Length) := Name;
         The_Name (The_Name'Last) := ASCII.NUL;

         Canonical_File_Addr := To_Canonical_File_Spec (The_Name'Address);
         Canonical_File_Len  := Strlen (Canonical_File_Addr);

         --  If VMS syntax conversion has failed, return an empty string
         --  to indicate the failure.

         if Canonical_File_Len = 0 then
            return "";
         end if;

         declare
            subtype Path_String is String (1 .. Canonical_File_Len);
            type    Path_String_Access is access Path_String;

            function Address_To_Access is new
               Unchecked_Conversion (Source => Address,
                                     Target => Path_String_Access);

            Path_Access : constant Path_String_Access :=
                            Address_To_Access (Canonical_File_Addr);

         begin
            Path_Buffer (1 .. Canonical_File_Len) := Path_Access.all;
            End_Path := Canonical_File_Len;
            Last := 1;
         end;
      end VMS_Conversion;

      --  Replace all '/' by Directory Separators (this is for Windows)

      if Directory_Separator /= '/' then
         for Index in 1 .. End_Path loop
            if Path_Buffer (Index) = '/' then
               Path_Buffer (Index) := Directory_Separator;
            end if;
         end loop;
      end if;

      --  Resolving logical names from VMS.
      --  If we have a Unix path on VMS such as /temp/..., and TEMP is a
      --  logical name, we need to resolve this logical name.
      --  As we have no means to know if we are on VMS, we need to do that
      --  for absolute paths starting with '/'.
      --  We find the directory, change to it, get the current directory,
      --  and change the directory to this value.

      if Path_Buffer (1) = '/' then
         declare
            Cur_Dir : String := Get_Directory ("");
            --  Save the current directory, so that we can change dir back to
            --  it. It is not a constant, because the last character (a
            --  directory separator) is changed to ASCII.NUL to call the C
            --  function chdir.

            Path : String := Path_Buffer (1 .. End_Path + 1);
            --  Copy of the current path. One character is added that may be
            --  set to ASCII.NUL to call chdir.

            Pos : Positive := End_Path;
            --  Position of the last directory separator ('/')

            Status : Integer;
            --  Value returned by chdir

         begin
            --  Look for the last '/'

            while Path (Pos) /= '/' loop
               Pos := Pos - 1;
            end loop;

            --  Get the previous character that is not a '/'

            while Pos > 1 and then Path (Pos) = '/' loop
               Pos := Pos - 1;
            end loop;

            --  If we are at the start of the path, take the full path.
            --  It may be a file in the root directory, but it may also be
            --  a subdirectory of the root directory.

            if Pos = 1 then
               Pos := End_Path;
            end if;

            --  Add the ASCII.NUL to be able to call the C function chdir
            Path (Pos + 1) := ASCII.NUL;

            Status := Change_Dir (Path (1 .. Pos + 1));

            --  If Status is not zero, then we do nothing: this is a file
            --  path or it is not a valid directory path.

            if Status = 0 then
               declare
                  New_Dir : constant String := Get_Directory ("");
                  --  The directory path

                  New_Path : String (1 .. New_Dir'Length + End_Path - Pos);
                  --  The new complete path, that is built below

               begin
                  New_Path (1 .. New_Dir'Length) := New_Dir;
                  New_Path (New_Dir'Length + 1 .. New_Path'Last) :=
                    Path_Buffer (Pos + 1 .. End_Path);
                  End_Path := New_Path'Length;
                  Path_Buffer (1 .. End_Path) := New_Path;
               end;

               --  Back to where we were before

               Cur_Dir (Cur_Dir'Last) := ASCII.NUL;
               Status := Change_Dir (Cur_Dir);
            end if;
         end;
      end if;

      --  Start the conversions

      --  If this is not finished after Max_Iterations, give up and
      --  return an empty string.

      for J in 1 .. Max_Iterations loop

         --  If we don't have an absolute pathname, prepend
         --  the directory Reference_Dir.

         if Last = 1
           and then not Is_Absolute_Path (Path_Buffer (1 .. End_Path))
         then
            Path_Buffer
              (Reference_Dir'Last + 1 .. Reference_Dir'Length + End_Path) :=
                 Path_Buffer (1 .. End_Path);
            End_Path := Reference_Dir'Length + End_Path;
            Path_Buffer (1 .. Reference_Dir'Length) := Reference_Dir;
            Last := Reference_Dir'Length;
         end if;

         --  If name starts with "//", we may have a drive letter on Interix

         if Last = 1 and then End_Path >= 3 then
            Need_To_Check_Drive_Letter := (Path_Buffer (1 .. 2)) = "//";
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

         --  Find the end of the current field: last character
         --  or the one preceding the next directory separator.

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
               exit when Start < 1 or else
                 Path_Buffer (Start) = Directory_Separator;
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
               Status := Readlink (Path_Buffer'Address,
                                   Link_Buffer'Address,
                                   Link_Buffer'Length);
               Path_Buffer (Finish + 1) := Saved;
            end;

            --  Not a symbolic link, move to the next field, if any

            if Status <= 0 then
               Last := Finish + 1;

            --  Replace symbolic link with its value.

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

      --  This can happen when there is a circularity in the symbolic links:
      --  A is a symbolic link for B, which itself is a symbolic link, and
      --  the target of B or of another symbolic link target of B is A.
      --  In this case, we return an empty string to indicate failure to
      --  resolve.

      return "";
   end Normalize_Pathname;

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

   ----------
   -- Read --
   ----------

   function Read
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer
   is
   begin
      return Integer (System.CRTL.read
        (System.CRTL.int (FD), System.CRTL.chars (A), System.CRTL.int (N)));
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
      pragma Import (C, rename, "rename");

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

   ------------
   -- Setenv --
   ------------

   procedure Setenv (Name : String; Value : String) is
      F_Name  : String (1 .. Name'Length + 1);
      F_Value : String (1 .. Value'Length + 1);

      procedure Set_Env_Value (Name, Value : System.Address);
      pragma Import (C, Set_Env_Value, "__gnat_set_env_value");

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
      --  Call Spawn.

      N_Args : Argument_List (Args'Range);
      --  Normalized arguments

      -----------
      -- Spawn --
      -----------

      procedure Spawn (Args : Argument_List) is
         type Chars is array (Positive range <>) of aliased Character;
         type Char_Ptr is access constant Character;

         Command_Len : constant Positive := Program_Name'Length + 1
                                              + Args_Length (Args);
         Command_Last : Natural := 0;
         Command : aliased Chars (1 .. Command_Len);
         --  Command contains all characters of the Program_Name and Args,
         --  all terminated by ASCII.NUL characters

         Arg_List_Len : constant Positive := Args'Length + 2;
         Arg_List_Last : Natural := 0;
         Arg_List : aliased array (1 .. Arg_List_Len) of Char_Ptr;
         --  List with pointers to NUL-terminated strings of the
         --  Program_Name and the Args and terminated with a null pointer.
         --  We rely on the default initialization for the last null pointer.

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

            --  Move characters one at a time, because Command has
            --  aliased components.

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
            Pid     := Invalid_Pid;
            Result  := Portable_Spawn (Arg_List'Address);
         else
            Pid     := Portable_No_Block_Spawn (Arg_List'Address);
            Result  := Boolean'Pos (Pid /= Invalid_Pid);
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

      function Address_To_Access is new
        Unchecked_Conversion (Source => Address,
                              Target => Path_String_Access);

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
     (FD   : File_Descriptor;
      A    : System.Address;
      N    : Integer) return Integer
   is
   begin
      return Integer (System.CRTL.write
        (System.CRTL.int (FD), System.CRTL.chars (A), System.CRTL.int (N)));
   end Write;

end GNAT.OS_Lib;
