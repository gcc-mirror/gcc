------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            M A K E _ U T I L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Debug;
with Errutil;
with Osint;    use Osint;
with Output;   use Output;
with Opt;      use Opt;
with Table;

with Ada.Command_Line;           use Ada.Command_Line;

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.HTable;

package body Make_Util is

   ---------
   -- Add --
   ---------

   procedure Add
     (Option : String_Access;
      To     : in out String_List_Access;
      Last   : in out Natural)
   is
   begin
      if Last = To'Last then
         declare
            New_Options : constant String_List_Access :=
                            new String_List (1 .. To'Last * 2);

         begin
            New_Options (To'Range) := To.all;

            --  Set all elements of the original options to null to avoid
            --  deallocation of copies.

            To.all := (others => null);

            Free (To);
            To := New_Options;
         end;
      end if;

      Last := Last + 1;
      To (Last) := Option;
   end Add;

   procedure Add
     (Option : String;
      To     : in out String_List_Access;
      Last   : in out Natural)
   is
   begin
      Add (Option => new String'(Option), To => To, Last => Last);
   end Add;

   -------------------------
   -- Base_Name_Index_For --
   -------------------------

   function Base_Name_Index_For
     (Main            : String;
      Main_Index      : Int;
      Index_Separator : Character) return File_Name_Type
   is
      Result : File_Name_Type;

   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Base_Name (Main));

      --  Remove the extension, if any, that is the last part of the base name
      --  starting with a dot and following some characters.

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Len := J - 1;
            exit;
         end if;
      end loop;

      --  Add the index info, if index is different from 0

      if Main_Index > 0 then
         Add_Char_To_Name_Buffer (Index_Separator);

         declare
            Img : constant String := Main_Index'Img;
         begin
            Add_Str_To_Name_Buffer (Img (2 .. Img'Last));
         end;
      end if;

      Result := Name_Find;
      return Result;
   end Base_Name_Index_For;

   -----------------
   -- Create_Name --
   -----------------

   function Create_Name (Name : String) return File_Name_Type is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   function Create_Name (Name : String) return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   function Create_Name (Name : String) return Path_Name_Type is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   ---------------------------
   -- Ensure_Absolute_Path --
   ---------------------------

   procedure Ensure_Absolute_Path
     (Switch               : in out String_Access;
      Parent               : String;
      Do_Fail              : Fail_Proc;
      For_Gnatbind         : Boolean := False;
      Including_Non_Switch : Boolean := True;
      Including_RTS        : Boolean := False)
   is
   begin
      if Switch /= null then
         declare
            Sw    : String (1 .. Switch'Length);
            Start : Positive;

         begin
            Sw := Switch.all;

            if Sw (1) = '-' then
               if Sw'Length >= 3
                 and then (Sw (2) = 'I'
                            or else (not For_Gnatbind
                                      and then (Sw (2) = 'L'
                                                 or else
                                                Sw (2) = 'A')))
               then
                  Start := 3;

                  if Sw = "-I-" then
                     return;
                  end if;

               elsif Sw'Length >= 4
                 and then
                   (Sw (2 .. 3) = "aL" or else
                    Sw (2 .. 3) = "aO" or else
                    Sw (2 .. 3) = "aI"
                      or else (For_Gnatbind and then Sw (2 .. 3) = "A="))
               then
                  Start := 4;

               elsif Including_RTS
                 and then Sw'Length >= 7
                 and then Sw (2 .. 6) = "-RTS="
               then
                  Start := 7;

               else
                  return;
               end if;

               --  Because relative path arguments to --RTS= may be relative to
               --  the search directory prefix, those relative path arguments
               --  are converted only when they include directory information.

               if not Is_Absolute_Path (Sw (Start .. Sw'Last)) then
                  if Parent'Length = 0 then
                     Do_Fail
                       ("relative search path switches ("""
                        & Sw
                        & """) are not allowed");

                  elsif Including_RTS then
                     for J in Start .. Sw'Last loop
                        if Sw (J) = Directory_Separator then
                           Switch :=
                             new String'
                               (Sw (1 .. Start - 1)
                                & Parent
                                & Directory_Separator
                                & Sw (Start .. Sw'Last));
                           return;
                        end if;
                     end loop;

                  else
                     Switch :=
                       new String'
                         (Sw (1 .. Start - 1)
                          & Parent
                          & Directory_Separator
                          & Sw (Start .. Sw'Last));
                  end if;
               end if;

            elsif Including_Non_Switch then
               if not Is_Absolute_Path (Sw) then
                  if Parent'Length = 0 then
                     Do_Fail
                       ("relative paths (""" & Sw & """) are not allowed");
                  else
                     Switch := new String'(Parent & Directory_Separator & Sw);
                  end if;
               end if;
            end if;
         end;
      end if;
   end Ensure_Absolute_Path;

   ----------------------------
   -- Executable_Prefix_Path --
   ----------------------------

   function Executable_Prefix_Path return String is
      Exec_Name : constant String := Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory where "bin"
      --  lies (in the example "C:\usr"). If the executable is not in a "bin"
      --  directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := S;
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Exec (J) = Directory_Separator then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                     and then Exec (Path_Last - 3) /= Directory_Separator)
         then
            return "";
         end if;

         return Normalize_Pathname
                  (Exec (Exec'First .. Path_Last - 4),
                   Resolve_Links => Opt.Follow_Links_For_Dirs)
           & Directory_Separator;
      end Get_Install_Dir;

   --  Beginning of Executable_Prefix_Path

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Exec_Name (J) = Directory_Separator then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If we get here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Path : String_Access := Locate_Exec_On_Path (Exec_Name);
      begin
         if Path = null then
            return "";
         else
            declare
               Dir : constant String := Get_Install_Dir (Path.all);
            begin
               Free (Path);
               return Dir;
            end;
         end if;
      end;
   end Executable_Prefix_Path;

   ------------------
   -- Fail_Program --
   ------------------

   procedure Fail_Program
     (S              : String;
      Flush_Messages : Boolean := True)
   is
   begin
      if Flush_Messages and not No_Exit_Message then
         if Total_Errors_Detected /= 0 or else Warnings_Detected /= 0 then
            Errutil.Finalize;
         end if;
      end if;

      Finish_Program (E_Fatal, S => S);
   end Fail_Program;

   --------------------
   -- Finish_Program --
   --------------------

   procedure Finish_Program
     (Exit_Code    : Osint.Exit_Code_Type := Osint.E_Success;
      S            : String := "")
   is
   begin
      if S'Length > 0 then
         if Exit_Code /= E_Success then
            if No_Exit_Message then
               Osint.Exit_Program (E_Fatal);
            else
               Osint.Fail (S);
            end if;

         elsif not No_Exit_Message then
            Write_Str (S);
         end if;
      end if;

      --  Output Namet statistics

      Namet.Finalize;

      Exit_Program (Exit_Code);
   end Finish_Program;

   ----------
   -- Hash --
   ----------

   function Hash is new GNAT.HTable.Hash (Header_Num => Header_Num);
   --  Used in implementation of other functions Hash below

   ----------
   -- Hash --
   ----------

   function Hash (Name : File_Name_Type) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
   end Hash;

   function Hash (Name : Name_Id) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
   end Hash;

   function Hash (Name : Path_Name_Type) return Header_Num is
   begin
      return Hash (Get_Name_String (Name));
   end Hash;

   ------------
   -- Inform --
   ------------

   procedure Inform (N : File_Name_Type; Msg : String) is
   begin
      Inform (Name_Id (N), Msg);
   end Inform;

   procedure Inform (N : Name_Id := No_Name; Msg : String) is
   begin
      Osint.Write_Program_Name;

      Write_Str (": ");

      if N /= No_Name then
         Write_Str ("""");

         declare
            Name : constant String := Get_Name_String (N);
         begin
            if Debug.Debug_Flag_F and then Is_Absolute_Path (Name) then
               Write_Str (File_Name (Name));
            else
               Write_Str (Name);
            end if;
         end;

         Write_Str (""" ");
      end if;

      Write_Str (Msg);
      Write_Eol;
   end Inform;

   -----------
   -- Mains --
   -----------

   package body Mains is

      package Names is new Table.Table
        (Table_Component_Type => Main_Info,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100,
         Table_Name           => "Makeutl.Mains.Names");
      --  The table that stores the mains

      Current : Natural := 0;
      --  The index of the last main retrieved from the table

      Count_Of_Mains_With_No_Tree : Natural := 0;
      --  Number of main units for which we do not know the project tree

      --------------
      -- Add_Main --
      --------------

      procedure Add_Main (Name : String; Index : Int := 0) is
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

         Names.Increment_Last;
         Names.Table (Names.Last) := (Name_Find, Index);

         Mains.Count_Of_Mains_With_No_Tree :=
           Mains.Count_Of_Mains_With_No_Tree + 1;
      end Add_Main;

      ------------
      -- Delete --
      ------------

      procedure Delete is
      begin
         Names.Set_Last (0);
         Mains.Reset;
      end Delete;

      ---------------
      -- Next_Main --
      ---------------

      function Next_Main return String is
         Info : constant Main_Info := Next_Main;
      begin
         if Info = No_Main_Info then
            return "";
         else
            return Get_Name_String (Info.File);
         end if;
      end Next_Main;

      function Next_Main return Main_Info is
      begin
         if Current >= Names.Last then
            return No_Main_Info;
         else
            Current := Current + 1;

            declare
               Orig_Main : constant File_Name_Type :=
                 Names.Table (Current).File;
               Current_Main : File_Name_Type;

            begin
               if Strip_Suffix (Orig_Main) = Orig_Main then
                  Get_Name_String (Orig_Main);
                  Add_Str_To_Name_Buffer (".adb");
                  Current_Main := Name_Find;

                  if Full_Source_Name (Current_Main) = No_File then
                     Get_Name_String (Orig_Main);
                     Add_Str_To_Name_Buffer (".ads");
                     Current_Main := Name_Find;

                     if Full_Source_Name (Current_Main) /= No_File then
                        Names.Table (Current).File := Current_Main;
                     end if;

                  else
                     Names.Table (Current).File := Current_Main;
                  end if;
               end if;
            end;

            return Names.Table (Current);
         end if;
      end Next_Main;

      ---------------------
      -- Number_Of_Mains --
      ---------------------

      function Number_Of_Mains return Natural is
      begin
         return Names.Last;
      end Number_Of_Mains;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Current := 0;
      end Reset;

      --------------------------
      -- Set_Multi_Unit_Index --
      --------------------------

      procedure Set_Multi_Unit_Index
        (Index        : Int := 0)
      is
      begin
         if Index /= 0 then
            if Names.Last = 0 then
               Fail_Program
                 ("cannot specify a multi-unit index but no main "
                  & "on the command line");

            elsif Names.Last > 1 then
               Fail_Program
                 ("cannot specify several mains with a multi-unit index");

            else
               Names.Table (Names.Last).Index := Index;
            end if;
         end if;
      end Set_Multi_Unit_Index;

   end Mains;

   -----------------------
   -- Path_Or_File_Name --
   -----------------------

   function Path_Or_File_Name (Path : Path_Name_Type) return String is
      Path_Name : constant String := Get_Name_String (Path);
   begin
      if Debug.Debug_Flag_F then
         return File_Name (Path_Name);
      else
         return Path_Name;
      end if;
   end Path_Or_File_Name;

   -------------------
   -- Unit_Index_Of --
   -------------------

   function Unit_Index_Of (ALI_File : File_Name_Type) return Int is
      Start  : Natural;
      Finish : Natural;
      Result : Int := 0;

   begin
      Get_Name_String (ALI_File);

      --  First, find the last dot

      Finish := Name_Len;

      while Finish >= 1 and then Name_Buffer (Finish) /= '.' loop
         Finish := Finish - 1;
      end loop;

      if Finish = 1 then
         return 0;
      end if;

      --  Now check that the dot is preceded by digits

      Start := Finish;
      Finish := Finish - 1;
      while Start >= 1 and then Name_Buffer (Start - 1) in '0' .. '9' loop
         Start := Start - 1;
      end loop;

      --  If there are no digits, or if the digits are not preceded by the
      --  character that precedes a unit index, this is not the ALI file of
      --  a unit in a multi-unit source.

      if Start > Finish
        or else Start = 1
        or else Name_Buffer (Start - 1) /= Multi_Unit_Index_Character
      then
         return 0;
      end if;

      --  Build the index from the digit(s)

      while Start <= Finish loop
         Result := Result * 10 +
                     Character'Pos (Name_Buffer (Start)) - Character'Pos ('0');
         Start := Start + 1;
      end loop;

      return Result;
   end Unit_Index_Of;

   -----------------
   -- Verbose_Msg --
   -----------------

   procedure Verbose_Msg
     (N1                : Name_Id;
      S1                : String;
      N2                : Name_Id := No_Name;
      S2                : String  := "";
      Prefix            : String := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low)
   is
   begin
      if not Opt.Verbose_Mode
        or else Minimum_Verbosity > Opt.Verbosity_Level
      then
         return;
      end if;

      Write_Str (Prefix);
      Write_Str ("""");
      Write_Name (N1);
      Write_Str (""" ");
      Write_Str (S1);

      if N2 /= No_Name then
         Write_Str (" """);
         Write_Name (N2);
         Write_Str (""" ");
      end if;

      Write_Str (S2);
      Write_Eol;
   end Verbose_Msg;

   procedure Verbose_Msg
     (N1                : File_Name_Type;
      S1                : String;
      N2                : File_Name_Type := No_File;
      S2                : String  := "";
      Prefix            : String := "  -> ";
      Minimum_Verbosity : Opt.Verbosity_Level_Type := Opt.Low)
   is
   begin
      Verbose_Msg
        (Name_Id (N1), S1, Name_Id (N2), S2, Prefix, Minimum_Verbosity);
   end Verbose_Msg;

   -----------
   -- Queue --
   -----------

   package body Queue is

      type Q_Record is record
         Info      : Source_Info;
         Processed : Boolean;
      end record;

      package Q is new Table.Table
        (Table_Component_Type => Q_Record,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 1000,
         Table_Increment      => 100,
         Table_Name           => "Makeutl.Queue.Q");
      --  This is the actual Queue

      type Mark_Key is record
         File  : File_Name_Type;
         Index : Int;
      end record;
      --  Identify either a mono-unit source (when Index = 0) or a specific
      --  unit (index = 1's origin index of unit) in a multi-unit source.

      Max_Mask_Num : constant := 2048;
      subtype Mark_Num is Union_Id range 0 .. Max_Mask_Num - 1;

      function Hash (Key : Mark_Key) return Mark_Num;

      package Marks is new GNAT.HTable.Simple_HTable
        (Header_Num => Mark_Num,
         Element    => Boolean,
         No_Element => False,
         Key        => Mark_Key,
         Hash       => Hash,
         Equal      => "=");
      --  A hash table to keep tracks of the marked units.
      --  These are the units that have already been processed, when using the
      --  gnatmake format. When using the gprbuild format, we can directly
      --  store in the source_id whether the file has already been processed.

      procedure Mark (Source_File : File_Name_Type; Index : Int := 0);
      --  Mark a unit, identified by its source file and, when Index is not 0,
      --  the index of the unit in the source file. Marking is used to signal
      --  that the unit has already been inserted in the Q.

      function Is_Marked
        (Source_File : File_Name_Type;
         Index       : Int := 0) return Boolean;
      --  Returns True if the unit was previously marked

      Q_Processed   : Natural := 0;
      Q_Initialized : Boolean := False;

      Q_First : Natural := 1;
      --  Points to the first valid element in the queue

      procedure Debug_Display (S : Source_Info);
      --  A debug display for S

      function Was_Processed (S : Source_Info) return Boolean;
      --  Whether S has already been processed. This marks the source as
      --  processed, if it hasn't already been processed.

      -------------------
      -- Was_Processed --
      -------------------

      function Was_Processed (S : Source_Info) return Boolean is
      begin
         if Is_Marked (S.File, S.Index) then
            return True;
         end if;

         Mark (S.File, Index => S.Index);

         return False;
      end Was_Processed;

      -------------------
      -- Debug_Display --
      -------------------

      procedure Debug_Display (S : Source_Info) is
      begin
         Write_Name (S.File);

         if S.Index /= 0 then
            Write_Str (", ");
            Write_Int (S.Index);
         end if;
      end Debug_Display;

      ----------
      -- Hash --
      ----------

      function Hash (Key : Mark_Key) return Mark_Num is
      begin
         return Union_Id (Key.File) mod Max_Mask_Num;
      end Hash;

      ---------------
      -- Is_Marked --
      ---------------

      function Is_Marked
        (Source_File : File_Name_Type;
         Index       : Int := 0) return Boolean
      is
      begin
         return Marks.Get (K => (File => Source_File, Index => Index));
      end Is_Marked;

      ----------
      -- Mark --
      ----------

      procedure Mark (Source_File : File_Name_Type; Index : Int := 0) is
      begin
         Marks.Set (K => (File => Source_File, Index => Index), E => True);
      end Mark;

      -------------
      -- Extract --
      -------------

      procedure Extract
        (Found  : out Boolean;
         Source : out Source_Info)
      is
      begin
         Found := False;

         if Q_First <= Q.Last then
            Source := Q.Table (Q_First).Info;
            Q.Table (Q_First).Processed := True;
            Q_First := Q_First + 1;
            Found := True;
         end if;

         if Found then
            Q_Processed := Q_Processed + 1;
         end if;

         if Found and then Debug.Debug_Flag_Q then
            Write_Str ("   Q := Q - [ ");
            Debug_Display (Source);
            Write_Str (" ]");
            Write_Eol;

            Write_Str ("   Q_First =");
            Write_Int (Int (Q_First));
            Write_Eol;

            Write_Str ("   Q.Last =");
            Write_Int (Int (Q.Last));
            Write_Eol;
         end if;
      end Extract;

      ---------------
      -- Processed --
      ---------------

      function Processed return Natural is
      begin
         return Q_Processed;
      end Processed;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Force : Boolean := False) is
      begin
         if Force or else not Q_Initialized then
            Q_Initialized := True;
            Q.Init;
            Q_Processed := 0;
            Q_First     := 1;
         end if;
      end Initialize;

      ------------
      -- Insert --
      ------------

      function Insert (Source  : Source_Info) return Boolean is
      begin
         --  Only insert in the Q if it is not already done, to avoid
         --  simultaneous compilations if -jnnn is used.

         if Was_Processed (Source) then
            return False;
         end if;

         Q.Append (New_Val => (Info => Source, Processed => False));

         if Debug.Debug_Flag_Q then
            Write_Str ("   Q := Q + [ ");
            Debug_Display (Source);
            Write_Str (" ] ");
            Write_Eol;

            Write_Str ("   Q_First =");
            Write_Int (Int (Q_First));
            Write_Eol;

            Write_Str ("   Q.Last =");
            Write_Int (Int (Q.Last));
            Write_Eol;
         end if;

         return True;
      end Insert;

      procedure Insert (Source : Source_Info) is
         Discard : Boolean;
      begin
         Discard := Insert (Source);
      end Insert;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Q_Processed >= Q.Last;
      end Is_Empty;

      ----------
      -- Size --
      ----------

      function Size return Natural is
      begin
         return Q.Last;
      end Size;

      -------------
      -- Element --
      -------------

      function Element (Rank : Positive) return File_Name_Type is
      begin
         if Rank <= Q.Last then
            return Q.Table (Rank).Info.File;
         else
            return No_File;
         end if;
      end Element;

      ------------------
      -- Remove_Marks --
      ------------------

      procedure Remove_Marks is
      begin
         Marks.Reset;
      end Remove_Marks;

   end Queue;

end Make_Util;
