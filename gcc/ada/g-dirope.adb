------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--            Copyright (C) 1998-2001 Ada Core Technologies, Inc.           --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Strings.Maps;
with Unchecked_Deallocation;
with Unchecked_Conversion;
with System;  use System;

with GNAT.Regexp;
with GNAT.OS_Lib;

package body GNAT.Directory_Operations is

   use Ada;

   type Dir_Type_Value is new System.Address;
   --  This is the low-level address directory structure as returned by the C
   --  opendir routine.

   Dir_Seps : constant Strings.Maps.Character_Set :=
                Strings.Maps.To_Set ("/\");
   --  UNIX and DOS style directory separators.

   procedure Free is new
     Unchecked_Deallocation (Dir_Type_Value, Dir_Type);

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (Path   : Path_Name;
      Suffix : String    := "")
      return   String
   is
      function Get_File_Names_Case_Sensitive return Integer;
      pragma Import
        (C, Get_File_Names_Case_Sensitive,
         "__gnat_get_file_names_case_sensitive");

      Case_Sensitive_File_Name : constant Boolean :=
                                   Get_File_Names_Case_Sensitive = 1;

      function Basename
        (Path   : Path_Name;
         Suffix : String    := "")
         return String;
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
         Suffix : String    := "")
         return   String
      is
         Cut_Start : Natural :=
                       Strings.Fixed.Index
                         (Path, Dir_Seps, Going => Strings.Backward);
         Cut_End : Natural;

      begin
         --  Cut_Start point to the first basename character

         if Cut_Start = 0 then
            Cut_Start := Path'First;

         else
            Cut_Start := Cut_Start + 1;
         end if;

         --  Cut_End point to the last basename character.

         Cut_End := Path'Last;

         --  If basename ends with Suffix, adjust Cut_End.

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

         begin
            if BN = "." or else BN = ".." then
               return "";

            elsif BN'Length > 2
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

   --  Start processing for Base_Name

   begin
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
      C_Dir_Name : String := Dir_Name & ASCII.NUL;

      function chdir (Dir_Name : String) return Integer;
      pragma Import (C, chdir, "chdir");

   begin
      if chdir (C_Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Change_Dir;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Dir_Type) is

      function closedir (Directory : System.Address) return Integer;
      pragma Import (C, closedir, "closedir");

      Discard : Integer;

   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Discard := closedir (System.Address (Dir.all));
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

   function Expand_Path (Path : Path_Name) return String is
      use Ada.Strings.Unbounded;

      procedure Read (K : in out Positive);
      --  Update Result while reading current Path starting at position K. If
      --  a variable is found, call Var below.

      procedure Var (K : in out Positive);
      --  Translate variable name starting at position K with the associated
      --  environment value.

      procedure Free is
         new Unchecked_Deallocation (String, OS_Lib.String_Access);

      Result : Unbounded_String;

      ----------
      -- Read --
      ----------

      procedure Read (K : in out Positive) is
      begin
         For_All_Characters : loop
            if Path (K) = '$' then

               --  Could be a variable

               if K < Path'Last then

                  if Path (K + 1) = '$' then

                     --  Not a variable after all, this is a double $, just
                     --  insert one in the result string.

                     Append (Result, '$');
                     K := K + 1;

                  else
                     --  Let's parse the variable

                     K := K + 1;
                     Var (K);
                  end if;

               else
                  --  We have an ending $ sign

                  Append (Result, '$');
               end if;

            else
               --  This is a standard character, just add it to the result

               Append (Result, Path (K));
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
         E : Positive;

      begin
         if Path (K) = '{' then

            --  Look for closing } (curly bracket).

            E := K;

            loop
               E := E + 1;
               exit when Path (E) = '}' or else E = Path'Last;
            end loop;

            if Path (E) = '}' then

               --  OK found, translate with environment value

               declare
                  Env : OS_Lib.String_Access :=
                          OS_Lib.Getenv (Path (K + 1 .. E - 1));

               begin
                  Append (Result, Env.all);
                  Free (Env);
               end;

            else
               --  No closing curly bracket, not a variable after all or a
               --  syntax error, ignore it, insert string as-is.

               Append (Result, '$' & Path (K .. E));
            end if;

         else
            --  The variable name is everything from current position to first
            --  non letter/digit character.

            E := K;

            --  Check that first chartacter is a letter

            if Characters.Handling.Is_Letter (Path (E)) then
               E := E + 1;

               Var_Name : loop
                  exit Var_Name when E = Path'Last;

                  if Characters.Handling.Is_Letter (Path (E))
                    or else Characters.Handling.Is_Digit (Path (E))
                  then
                     E := E + 1;
                  else
                     E := E - 1;
                     exit Var_Name;
                  end if;
               end loop Var_Name;

               declare
                  Env : OS_Lib.String_Access := OS_Lib.Getenv (Path (K .. E));

               begin
                  Append (Result, Env.all);
                  Free (Env);
               end;

            else
               --  This is not a variable after all

               Append (Result, '$' & Path (E));
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
         return To_String (Result);
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

   ----------
   -- Find --
   ----------

   procedure Find
     (Root_Directory : Dir_Name_Str;
      File_Pattern   : String)
   is
      File_Regexp : constant Regexp.Regexp := Regexp.Compile (File_Pattern);
      Index       : Natural := 0;

      procedure Read_Directory (Directory : Dir_Name_Str);
      --  Open Directory and read all entries. This routine is called
      --  recursively for each sub-directories.

      function Make_Pathname (Dir, File : String) return String;
      --  Returns the pathname for File by adding Dir as prefix.

      -------------------
      -- Make_Pathname --
      -------------------

      function Make_Pathname (Dir, File : String) return String is
      begin
         if Dir (Dir'Last) = '/' or else Dir (Dir'Last) = '\' then
            return Dir & File;
         else
            return Dir & Dir_Separator & File;
         end if;
      end Make_Pathname;

      --------------------
      -- Read_Directory --
      --------------------

      procedure Read_Directory (Directory : Dir_Name_Str) is
         Dir    : Dir_Type;
         Buffer : String (1 .. 2_048);
         Last   : Natural;
         Quit   : Boolean;

      begin
         Open (Dir, Directory);

         loop
            Read (Dir, Buffer, Last);
            exit when Last = 0;

            declare
               Dir_Entry : constant String := Buffer (1 .. Last);
               Pathname  : constant String
                 := Make_Pathname (Directory, Dir_Entry);
            begin
               if Regexp.Match (Dir_Entry, File_Regexp) then
                  Quit  := False;
                  Index := Index + 1;

                  begin
                     Action (Pathname, Index, Quit);
                  exception
                     when others =>
                        Close (Dir);
                        raise;
                  end;

                  exit when Quit;
               end if;

               --  Recursively call for sub-directories, except for . and ..

               if not (Dir_Entry = "." or else Dir_Entry = "..")
                 and then OS_Lib.Is_Directory (Pathname)
               then
                  Read_Directory (Pathname);
               end if;
            end;
         end loop;

         Close (Dir);
      end Read_Directory;

   begin
      Read_Directory (Root_Directory);
   end Find;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   Max_Path : Integer;
   pragma Import (C, Max_Path, "max_path_len");

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

      if Dir'Length > Path_Len then
         Last := Dir'First + Path_Len - 1;
      else
         Last := Dir'Last;
      end if;

      Dir (Buffer'First .. Last) := Buffer (Buffer'First .. Last);
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
      C_Dir_Name : String := Dir_Name & ASCII.NUL;

      function mkdir (Dir_Name : String) return Integer;
      pragma Import (C, mkdir, "__gnat_mkdir");

   begin
      if mkdir (C_Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Make_Dir;

   ------------------------
   -- Normalize_Pathname --
   ------------------------

   function Normalize_Pathname
     (Path  : Path_Name;
      Style : Path_Style := System_Default)
      return  String
   is
      N_Path      : String := Path;
      K           : Positive := N_Path'First;
      Prev_Dirsep : Boolean := False;

   begin
      for J in Path'Range loop

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
   end Normalize_Pathname;

   ----------
   -- Open --
   ----------

   procedure Open
     (Dir      : out Dir_Type;
      Dir_Name : Dir_Name_Str)
   is
      C_File_Name : String := Dir_Name & ASCII.NUL;

      function opendir
        (File_Name : String)
         return      Dir_Type_Value;
      pragma Import (C, opendir, "opendir");

   begin
      Dir := new Dir_Type_Value'(opendir (C_File_Name));

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
     (Dir  : in out Dir_Type;
      Str  : out String;
      Last : out Natural)
   is
      Filename_Addr : Address;
      Filename_Len  : Integer;

      Buffer : array (0 .. 1024) of Character;
      --  1024 is the value of FILENAME_MAX in stdio.h

      function readdir_gnat
        (Directory : System.Address;
         Buffer    : System.Address)
         return      System.Address;
      pragma Import (C, readdir_gnat, "__gnat_readdir");

      function strlen (S : Address) return Integer;
      pragma Import (C, strlen, "strlen");

   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Filename_Addr :=
        readdir_gnat (System.Address (Dir.all), Buffer'Address);

      if Filename_Addr = System.Null_Address then
         Last := 0;
         return;
      end if;

      Filename_Len  := strlen (Filename_Addr);

      if Str'Length > Filename_Len then
         Last := Str'First + Filename_Len - 1;
      else
         Last := Str'Last;
      end if;

      declare
         subtype Path_String is String (1 .. Filename_Len);
         type    Path_String_Access is access Path_String;

         function Address_To_Access is new
           Unchecked_Conversion
             (Source => Address,
              Target => Path_String_Access);

         Path_Access : Path_String_Access := Address_To_Access (Filename_Addr);

      begin
         for J in Str'First .. Last loop
            Str (J) := Path_Access (J - Str'First + 1);
         end loop;
      end;
   end Read;

   -------------------------
   -- Read_Is_Thread_Sage --
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

   procedure Remove_Dir (Dir_Name : Dir_Name_Str) is
      C_Dir_Name : String := Dir_Name & ASCII.NUL;

      procedure rmdir (Dir_Name : String);
      pragma Import (C, rmdir, "rmdir");

   begin
      rmdir (C_Dir_Name);
   end Remove_Dir;

   -----------------------
   -- Wildcard_Iterator --
   -----------------------

   procedure Wildcard_Iterator (Path : Path_Name) is

      Index : Natural := 0;

      procedure Read
        (Directory      : String;
         File_Pattern   : String;
         Suffix_Pattern : String);
      --  Read entries in Directory and call user's callback if the entry
      --  match File_Pattern and Suffix_Pattern is empty otherwise it will go
      --  down one more directory level by calling Next_Level routine above.

      procedure Next_Level
        (Current_Path : String;
         Suffix_Path  : String);
      --  Extract next File_Pattern from Suffix_Path and call Read routine
      --  above.

      ----------------
      -- Next_Level --
      ----------------

      procedure Next_Level
        (Current_Path : String;
         Suffix_Path  : String)
      is
         DS : Natural;
         SP : String renames Suffix_Path;

      begin
         if SP'Length > 2
           and then SP (SP'First) = '.'
           and then Strings.Maps.Is_In (SP (SP'First + 1), Dir_Seps)
         then
            --  Starting with "./"

            DS := Strings.Fixed.Index
              (SP (SP'First + 2 .. SP'Last),
               Dir_Seps);

            if DS = 0 then

               --  We have "./"

               Read (Current_Path & ".", "*", "");

            else
               --  We have "./dir"

               Read (Current_Path & ".",
                     SP (SP'First + 2 .. DS - 1),
                     SP (DS .. SP'Last));
            end if;

         elsif SP'Length > 3
           and then SP (SP'First .. SP'First + 1) = ".."
           and then Strings.Maps.Is_In (SP (SP'First + 2), Dir_Seps)
         then
            --  Starting with "../"

            DS := Strings.Fixed.Index
              (SP (SP'First + 3 .. SP'Last),
               Dir_Seps);

            if DS = 0 then

               --  We have "../"

               Read (Current_Path & "..", "*", "");

            else
               --  We have "../dir"

               Read (Current_Path & "..",
                     SP (SP'First + 4 .. DS - 1),
                     SP (DS .. SP'Last));
            end if;

         elsif Current_Path = ""
           and then SP'Length > 1
           and then Characters.Handling.Is_Letter (SP (SP'First))
           and then SP (SP'First + 1) = ':'
         then
            --  Starting with "<drive>:"

            if SP'Length > 2
              and then Strings.Maps.Is_In (SP (SP'First + 2), Dir_Seps)
            then
               --  Starting with "<drive>:\"

               DS :=  Strings.Fixed.Index
                        (SP (SP'First + 3 .. SP'Last), Dir_Seps);

               if DS = 0 then

                  --  Se have "<drive>:\dir"

                  Read (SP (SP'First .. SP'First + 1),
                        SP (SP'First + 3 .. SP'Last),
                        "");

               else
                  --  We have "<drive>:\dir\kkk"

                  Read (SP (SP'First .. SP'First + 1),
                        SP (SP'First + 3 .. DS - 1),
                        SP (DS .. SP'Last));
               end if;

            else
               --  Starting with "<drive>:"

               DS :=  Strings.Fixed.Index
                        (SP (SP'First + 2 .. SP'Last), Dir_Seps);

               if DS = 0 then

                  --  We have "<drive>:dir"

                  Read (SP (SP'First .. SP'First + 1),
                        SP (SP'First + 2 .. SP'Last),
                        "");

               else
                  --  We have "<drive>:dir/kkk"

                  Read (SP (SP'First .. SP'First + 1),
                        SP (SP'First + 2 .. DS - 1),
                        SP (DS .. SP'Last));
               end if;

            end if;

         elsif Strings.Maps.Is_In (SP (SP'First), Dir_Seps) then

            --  Starting with a /

            DS := Strings.Fixed.Index
              (SP (SP'First + 1 .. SP'Last),
               Dir_Seps);

            if DS = 0 then

               --  We have "/dir"

               Read (Current_Path,
                     SP (SP'First + 1 .. SP'Last),
                     "");
            else
               --  We have "/dir/kkk"

               Read (Current_Path,
                     SP (SP'First + 1 .. DS - 1),
                     SP (DS .. SP'Last));
            end if;

         else
            --  Starting with a name

            DS := Strings.Fixed.Index (SP, Dir_Seps);

            if DS = 0 then

               --  We have "dir"

               Read (Current_Path & '.',
                     SP,
                     "");
            else
               --  We have "dir/kkk"

               Read (Current_Path & '.',
                     SP (SP'First .. DS - 1),
                     SP (DS .. SP'Last));
            end if;

         end if;
      end Next_Level;

      ----------
      -- Read --
      ----------

      Quit : Boolean := False;
      --  Global state to be able to exit all recursive calls.

      procedure Read
        (Directory      : String;
         File_Pattern   : String;
         Suffix_Pattern : String)
      is
         File_Regexp : constant Regexp.Regexp :=
                         Regexp.Compile (File_Pattern, Glob => True);
         Dir    : Dir_Type;
         Buffer : String (1 .. 2_048);
         Last   : Natural;

      begin
         if OS_Lib.Is_Directory (Directory) then
            Open (Dir, Directory);

            Dir_Iterator : loop
               Read (Dir, Buffer, Last);
               exit Dir_Iterator when Last = 0;

               declare
                  Dir_Entry : constant String := Buffer (1 .. Last);
                  Pathname  : constant String :=
                                Directory & Dir_Separator & Dir_Entry;
               begin
                  --  Handle "." and ".." only if explicit use in the
                  --  File_Pattern.

                  if not
                    ((Dir_Entry = "." and then File_Pattern /= ".")
                       or else
                     (Dir_Entry = ".." and then File_Pattern /= ".."))
                  then
                     if Regexp.Match (Dir_Entry, File_Regexp) then

                        if Suffix_Pattern = "" then

                           --  No more matching needed, call user's callback

                           Index := Index + 1;

                           begin
                              Action (Pathname, Index, Quit);

                           exception
                              when others =>
                                 Close (Dir);
                                 raise;
                           end;

                           exit Dir_Iterator when Quit;

                        else
                           --  Down one level

                           Next_Level
                             (Directory & Dir_Separator & Dir_Entry,
                              Suffix_Pattern);
                        end if;
                     end if;
                  end if;
               end;

               exit Dir_Iterator when Quit;

            end loop Dir_Iterator;

            Close (Dir);
         end if;
      end Read;

   begin
      Next_Level ("", Path);
   end Wildcard_Iterator;

end GNAT.Directory_Operations;
