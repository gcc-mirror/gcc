------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1998-2017, AdaCore                     --
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

with Ada.IO_Exceptions;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with System;      use System;
with System.CRTL; use System.CRTL;

with GNAT.OS_Lib;

package body GNAT.Directory_Operations is

   use Ada;

   Filename_Max : constant Integer := 1024;
   --  1024 is the value of FILENAME_MAX in stdio.h

   procedure Free is new
     Ada.Unchecked_Deallocation (Dir_Type_Value, Dir_Type);

   On_Windows : constant Boolean := GNAT.OS_Lib.Directory_Separator = '\';
   --  An indication that we are on Windows. Used in Get_Current_Dir, to
   --  deal with drive letters in the beginning of absolute paths.

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (Path   : Path_Name;
      Suffix : String := "") return String
   is
      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");

      Case_Sensitive_File_Name : constant Boolean :=
                                   Get_File_Names_Case_Sensitive = 1;

      function Basename
        (Path   : Path_Name;
         Suffix : String := "") return String;
      --  This function does the job. The only difference between Basename
      --  and Base_Name (the parent function) is that the former is case
      --  sensitive, while the latter is not. Path and Suffix are adjusted
      --  appropriately before calling Basename under platforms where the
      --  file system is not case sensitive.

      --------------
      -- Basename --
      --------------

      function Basename
        (Path   : Path_Name;
         Suffix : String    := "") return String
      is
         Cut_Start : Natural :=
                       Strings.Fixed.Index
                         (Path, Dir_Seps, Going => Strings.Backward);
         Cut_End : Natural;

      begin
         --  Cut_Start point to the first basename character

         Cut_Start := (if Cut_Start = 0 then Path'First else Cut_Start + 1);

         --  Cut_End point to the last basename character

         Cut_End := Path'Last;

         --  If basename ends with Suffix, adjust Cut_End

         if Suffix /= ""
           and then Path (Path'Last - Suffix'Length + 1 .. Cut_End) = Suffix
         then
            Cut_End := Path'Last - Suffix'Length;
         end if;

         Check_For_Standard_Dirs : declare
            Offset : constant Integer := Path'First - Base_Name.Path'First;
            BN     : constant String  :=
                       Base_Name.Path (Cut_Start - Offset .. Cut_End - Offset);
            --  Here we use Base_Name.Path to keep the original casing

            Has_Drive_Letter : constant Boolean :=
                                 OS_Lib.Path_Separator /= ':';
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
      end Basename;

   --  Start of processing for Base_Name

   begin
      if Path'Length <= Suffix'Length then
         return Path;
      end if;

      if Case_Sensitive_File_Name then
         return Basename (Path, Suffix);
      else
         return Basename
           (Characters.Handling.To_Lower (Path),
            Characters.Handling.To_Lower (Suffix));
      end if;
   end Base_Name;

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : Dir_Name_Str) is
      C_Dir_Name : constant String := Dir_Name & ASCII.NUL;
   begin
      if chdir (C_Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Change_Dir;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Dir_Type) is
      Discard : Integer;
      pragma Warnings (Off, Discard);

      function closedir (directory : DIRs) return Integer;
      pragma Import (C, closedir, "__gnat_closedir");

   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Discard := closedir (DIRs (Dir.all));
      Free (Dir);
   end Close;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (Path : Path_Name) return Dir_Name_Str is
      Last_DS : constant Natural :=
                  Strings.Fixed.Index
                    (Path, Dir_Seps, Going => Strings.Backward);

   begin
      if Last_DS = 0 then

         --  There is no directory separator, returns current working directory

         return "." & Dir_Separator;

      else
         return Path (Path'First .. Last_DS);
      end if;
   end Dir_Name;

   -----------------
   -- Expand_Path --
   -----------------

   function Expand_Path
     (Path : Path_Name;
      Mode : Environment_Style := System_Default) return Path_Name
   is
      Environment_Variable_Char : Character;
      pragma Import (C, Environment_Variable_Char, "__gnat_environment_char");

      Result      : OS_Lib.String_Access := new String (1 .. 200);
      Result_Last : Natural := 0;

      procedure Append (C : Character);
      procedure Append (S : String);
      --  Append to Result

      procedure Double_Result_Size;
      --  Reallocate Result, doubling its size

      function Is_Var_Prefix (C : Character) return Boolean;
      pragma Inline (Is_Var_Prefix);

      procedure Read (K : in out Positive);
      --  Update Result while reading current Path starting at position K. If
      --  a variable is found, call Var below.

      procedure Var (K : in out Positive);
      --  Translate variable name starting at position K with the associated
      --  environment value.

      ------------
      -- Append --
      ------------

      procedure Append (C : Character) is
      begin
         if Result_Last = Result'Last then
            Double_Result_Size;
         end if;

         Result_Last := Result_Last + 1;
         Result (Result_Last) := C;
      end Append;

      procedure Append (S : String) is
      begin
         while Result_Last + S'Length - 1 > Result'Last loop
            Double_Result_Size;
         end loop;

         Result (Result_Last + 1 .. Result_Last + S'Length) := S;
         Result_Last := Result_Last + S'Length;
      end Append;

      ------------------------
      -- Double_Result_Size --
      ------------------------

      procedure Double_Result_Size is
         New_Result : constant OS_Lib.String_Access :=
                        new String (1 .. 2 * Result'Last);
      begin
         New_Result (1 .. Result_Last) := Result (1 .. Result_Last);
         OS_Lib.Free (Result);
         Result := New_Result;
      end Double_Result_Size;

      -------------------
      -- Is_Var_Prefix --
      -------------------

      function Is_Var_Prefix (C : Character) return Boolean is
      begin
         return (C = Environment_Variable_Char and then Mode = System_Default)
           or else
             (C = '$' and then (Mode = UNIX or else Mode = Both))
           or else
             (C = '%' and then (Mode = DOS or else Mode = Both));
      end Is_Var_Prefix;

      ----------
      -- Read --
      ----------

      procedure Read (K : in out Positive) is
         P : Character;

      begin
         For_All_Characters : loop
            if Is_Var_Prefix (Path (K)) then
               P := Path (K);

               --  Could be a variable

               if K < Path'Last then
                  if Path (K + 1) = P then

                     --  Not a variable after all, this is a double $ or %,
                     --  just insert one in the result string.

                     Append (P);
                     K := K + 1;

                  else
                     --  Let's parse the variable

                     Var (K);
                  end if;

               else
                  --  We have an ending $ or % sign

                  Append (P);
               end if;

            else
               --  This is a standard character, just add it to the result

               Append (Path (K));
            end if;

            --  Skip to next character

            K := K + 1;

            exit For_All_Characters when K > Path'Last;
         end loop For_All_Characters;
      end Read;

      ---------
      -- Var --
      ---------

      procedure Var (K : in out Positive) is
         P : constant Character := Path (K);
         T : Character;
         E : Positive;

      begin
         K := K + 1;

         if P = '%' or else Path (K) = '{' then

            --  Set terminator character

            if P = '%' then
               T := '%';
            else
               T := '}';
               K := K + 1;
            end if;

            --  Look for terminator character, k point to the first character
            --  for the variable name.

            E := K;

            loop
               E := E + 1;
               exit when Path (E) = T or else E = Path'Last;
            end loop;

            if Path (E) = T then

               --  OK found, translate with environment value

               declare
                  Env : OS_Lib.String_Access :=
                          OS_Lib.Getenv (Path (K .. E - 1));

               begin
                  Append (Env.all);
                  OS_Lib.Free (Env);
               end;

            else
               --  No terminator character, not a variable after all or a
               --  syntax error, ignore it, insert string as-is.

               Append (P);       --  Add prefix character

               if T = '}' then   --  If we were looking for curly bracket
                  Append ('{');  --  terminator, add the curly bracket
               end if;

               Append (Path (K .. E));
            end if;

         else
            --  The variable name is everything from current position to first
            --  non letter/digit character.

            E := K;

            --  Check that first character is a letter

            if Characters.Handling.Is_Letter (Path (E)) then
               E := E + 1;

               Var_Name : loop
                  exit Var_Name when E > Path'Last;

                  if Characters.Handling.Is_Letter (Path (E))
                    or else Characters.Handling.Is_Digit (Path (E))
                  then
                     E := E + 1;
                  else
                     exit Var_Name;
                  end if;
               end loop Var_Name;

               E := E - 1;

               declare
                  Env : OS_Lib.String_Access := OS_Lib.Getenv (Path (K .. E));

               begin
                  Append (Env.all);
                  OS_Lib.Free (Env);
               end;

            else
               --  This is not a variable after all

               Append ('$');
               Append (Path (E));
            end if;

         end if;

         K := E;
      end Var;

   --  Start of processing for Expand_Path

   begin
      declare
         K : Positive := Path'First;

      begin
         Read (K);

         declare
            Returned_Value : constant String := Result (1 .. Result_Last);

         begin
            OS_Lib.Free (Result);
            return Returned_Value;
         end;
      end;
   end Expand_Path;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (Path : Path_Name) return String is
      First : Natural :=
                Strings.Fixed.Index
                  (Path, Dir_Seps, Going => Strings.Backward);

      Dot : Natural;

   begin
      if First = 0 then
         First := Path'First;
      end if;

      Dot := Strings.Fixed.Index (Path (First .. Path'Last),
                                  ".",
                                  Going => Strings.Backward);

      if Dot = 0 or else Dot = Path'Last then
         return "";
      else
         return Path (Dot .. Path'Last);
      end if;
   end File_Extension;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Path : Path_Name) return String is
   begin
      return Base_Name (Path);
   end File_Name;

   ---------------------
   -- Format_Pathname --
   ---------------------

   function Format_Pathname
     (Path  : Path_Name;
      Style : Path_Style := System_Default) return String
   is
      N_Path       : String   := Path;
      K            : Positive := N_Path'First;
      Prev_Dirsep  : Boolean  := False;

   begin
      if Dir_Separator = '\'
        and then Path'Length > 1
        and then Path (K .. K + 1) = "\\"
      then
         if Style = UNIX then
            N_Path (K .. K + 1) := "//";
         end if;

         K := K + 2;
      end if;

      for J in K .. Path'Last loop
         if Strings.Maps.Is_In (Path (J), Dir_Seps) then
            if not Prev_Dirsep then
               case Style is
                  when UNIX           => N_Path (K) := '/';
                  when DOS            => N_Path (K) := '\';
                  when System_Default => N_Path (K) := Dir_Separator;
               end case;

               K := K + 1;
            end if;

            Prev_Dirsep := True;

         else
            N_Path (K) := Path (J);
            K := K + 1;
            Prev_Dirsep := False;
         end if;
      end loop;

      return N_Path (N_Path'First .. K - 1);
   end Format_Pathname;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   Max_Path : Integer;
   pragma Import (C, Max_Path, "__gnat_max_path_len");

   function Get_Current_Dir return Dir_Name_Str is
      Current_Dir : String (1 .. Max_Path + 1);
      Last        : Natural;
   begin
      Get_Current_Dir (Current_Dir, Last);
      return Current_Dir (1 .. Last);
   end Get_Current_Dir;

   procedure Get_Current_Dir (Dir : out Dir_Name_Str; Last : out Natural) is
      Path_Len : Natural := Max_Path;
      Buffer   : String (Dir'First .. Dir'First + Max_Path + 1);

      procedure Local_Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Local_Get_Current_Dir, "__gnat_get_current_dir");

   begin
      Local_Get_Current_Dir (Buffer'Address, Path_Len'Address);

      if Path_Len = 0 then
         raise Ada.IO_Exceptions.Use_Error
           with "current directory does not exist";
      end if;

      Last :=
        (if Dir'Length > Path_Len then Dir'First + Path_Len - 1 else Dir'Last);

      Dir (Buffer'First .. Last) := Buffer (Buffer'First .. Last);

      --  By default, the drive letter on Windows is in upper case

      if On_Windows and then Last > Dir'First and then
        Dir (Dir'First + 1) = ':'
      then
         Dir (Dir'First) :=
           Ada.Characters.Handling.To_Upper (Dir (Dir'First));
      end if;
   end Get_Current_Dir;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Dir : Dir_Type) return Boolean is
   begin
      return Dir /= Null_Dir
        and then System.Address (Dir.all) /= System.Null_Address;
   end Is_Open;

   --------------
   -- Make_Dir --
   --------------

   procedure Make_Dir (Dir_Name : Dir_Name_Str) is
      C_Dir_Name : constant String := Dir_Name & ASCII.NUL;
   begin
      if CRTL.mkdir (C_Dir_Name, Unspecified) /= 0 then
         raise Directory_Error;
      end if;
   end Make_Dir;

   ----------
   -- Open --
   ----------

   procedure Open
     (Dir      : out Dir_Type;
      Dir_Name : Dir_Name_Str)
   is
      function opendir (file_name : String) return DIRs;
      pragma Import (C, opendir, "__gnat_opendir");

      C_File_Name : constant String := Dir_Name & ASCII.NUL;

   begin
      Dir := new Dir_Type_Value'(Dir_Type_Value (opendir (C_File_Name)));

      if not Is_Open (Dir) then
         Free (Dir);
         Dir := Null_Dir;
         raise Directory_Error;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Dir  : Dir_Type;
      Str  : out String;
      Last : out Natural)
   is
      Filename_Addr : Address;
      Filename_Len  : aliased Integer;

      Buffer : array (0 .. Filename_Max + 12) of Character;
      --  12 is the size of the dirent structure (see dirent.h), without the
      --  field for the filename.

      function readdir_gnat
        (Directory : System.Address;
         Buffer    : System.Address;
         Last      : not null access Integer) return System.Address;
      pragma Import (C, readdir_gnat, "__gnat_readdir");

   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Filename_Addr :=
        readdir_gnat
          (System.Address (Dir.all), Buffer'Address, Filename_Len'Access);

      if Filename_Addr = System.Null_Address then
         Last := 0;
         return;
      end if;

      Last :=
        (if Str'Length > Filename_Len then Str'First + Filename_Len - 1
         else Str'Last);

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
         for J in Str'First .. Last loop
            Str (J) := Path_Access (J - Str'First + 1);
         end loop;
      end;
   end Read;

   -------------------------
   -- Read_Is_Thread_Safe --
   -------------------------

   function Read_Is_Thread_Safe return Boolean is
      function readdir_is_thread_safe return Integer;
      pragma Import
        (C, readdir_is_thread_safe, "__gnat_readdir_is_thread_safe");
   begin
      return (readdir_is_thread_safe /= 0);
   end Read_Is_Thread_Safe;

   ----------------
   -- Remove_Dir --
   ----------------

   procedure Remove_Dir
     (Dir_Name  : Dir_Name_Str;
      Recursive : Boolean := False)
   is
      C_Dir_Name  : constant String := Dir_Name & ASCII.NUL;
      Last        : Integer;
      Str         : String (1 .. Filename_Max);
      Success     : Boolean;
      Current_Dir : Dir_Type;

   begin
      --  Remove the directory only if it is empty

      if not Recursive then
         if rmdir (C_Dir_Name) /= 0 then
            raise Directory_Error;
         end if;

      --  Remove directory and all files and directories that it may contain

      else
         Open (Current_Dir, Dir_Name);

         loop
            Read (Current_Dir, Str, Last);
            exit when Last = 0;

            if GNAT.OS_Lib.Is_Directory
                 (Dir_Name & Dir_Separator &  Str (1 .. Last))
            then
               if Str (1 .. Last) /= "."
                 and then
                   Str (1 .. Last) /= ".."
               then
                  --  Recursive call to remove a subdirectory and all its
                  --  files.

                  Remove_Dir
                    (Dir_Name & Dir_Separator &  Str (1 .. Last),
                     True);
               end if;

            else
               GNAT.OS_Lib.Delete_File
                 (Dir_Name & Dir_Separator &  Str (1 .. Last),
                  Success);

               if not Success then
                  raise Directory_Error;
               end if;
            end if;
         end loop;

         Close (Current_Dir);
         Remove_Dir (Dir_Name);
      end if;
   end Remove_Dir;

end GNAT.Directory_Operations;
