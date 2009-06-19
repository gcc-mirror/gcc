------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M A K E U T L                               --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Debug;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Ext;
with Prj.Util;
with Snames;   use Snames;
with Table;

with Ada.Command_Line;  use Ada.Command_Line;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with System.Case_Util; use System.Case_Util;
with System.HTable;

package body Makeutl is

   type Mark_Key is record
      File  : File_Name_Type;
      Index : Int;
   end record;
   --  Identify either a mono-unit source (when Index = 0) or a specific unit
   --  (index = 1's origin index of unit) in a multi-unit source.

   --  There follow many global undocumented declarations, comments needed ???

   Max_Mask_Num : constant := 2048;

   subtype Mark_Num is Union_Id range 0 .. Max_Mask_Num - 1;

   function Hash (Key : Mark_Key) return Mark_Num;

   package Marks is new System.HTable.Simple_HTable
     (Header_Num => Mark_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Mark_Key,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to keep tracks of the marked units

   type Linker_Options_Data is record
      Project : Project_Id;
      Options : String_List_Id;
   end record;

   Linker_Option_Initial_Count : constant := 20;

   Linker_Options_Buffer : String_List_Access :=
     new String_List (1 .. Linker_Option_Initial_Count);

   Last_Linker_Option : Natural := 0;

   package Linker_Opts is new Table.Table (
     Table_Component_Type => Linker_Options_Data,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Make.Linker_Opts");

   procedure Add_Linker_Option (Option : String);

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

   -----------------------
   -- Add_Linker_Option --
   -----------------------

   procedure Add_Linker_Option (Option : String) is
   begin
      if Option'Length > 0 then
         if Last_Linker_Option = Linker_Options_Buffer'Last then
            declare
               New_Buffer : constant String_List_Access :=
                              new String_List
                                (1 .. Linker_Options_Buffer'Last +
                                        Linker_Option_Initial_Count);
            begin
               New_Buffer (Linker_Options_Buffer'Range) :=
                 Linker_Options_Buffer.all;
               Linker_Options_Buffer.all := (others => null);
               Free (Linker_Options_Buffer);
               Linker_Options_Buffer := New_Buffer;
            end;
         end if;

         Last_Linker_Option := Last_Linker_Option + 1;
         Linker_Options_Buffer (Last_Linker_Option) := new String'(Option);
      end if;
   end Add_Linker_Option;

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

   ----------------------
   -- Delete_All_Marks --
   ----------------------

   procedure Delete_All_Marks is
   begin
      Marks.Reset;
   end Delete_All_Marks;

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

         return Normalize_Pathname (Exec (Exec'First .. Path_Last - 4))
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

   ----------
   -- Hash --
   ----------

   function Hash (Key : Mark_Key) return Mark_Num is
   begin
      return Union_Id (Key.File) mod Max_Mask_Num;
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

   ----------------------------
   -- Is_External_Assignment --
   ----------------------------

   function Is_External_Assignment (Argv : String) return Boolean is
      Start     : Positive := 3;
      Finish    : Natural := Argv'Last;
      Equal_Pos : Natural;

      pragma Assert (Argv'First = 1);
      pragma Assert (Argv (1 .. 2) = "-X");

   begin
      if Argv'Last < 5 then
         return False;

      elsif Argv (3) = '"' then
         if Argv (Argv'Last) /= '"' or else Argv'Last < 7 then
            return False;
         else
            Start := 4;
            Finish := Argv'Last - 1;
         end if;
      end if;

      Equal_Pos := Start;

      while Equal_Pos <= Finish and then Argv (Equal_Pos) /= '=' loop
         Equal_Pos := Equal_Pos + 1;
      end loop;

      if Equal_Pos = Start or else Equal_Pos > Finish then
         return False;
      else
         Prj.Ext.Add
           (External_Name => Argv (Start .. Equal_Pos - 1),
            Value         => Argv (Equal_Pos + 1 .. Finish));
         return True;
      end if;
   end Is_External_Assignment;

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

   -----------------------------
   -- Linker_Options_Switches --
   -----------------------------

   function Linker_Options_Switches
     (Project  : Project_Id;
      In_Tree  : Project_Tree_Ref) return String_List
   is
      procedure Recursive_Add (Proj : Project_Id; Dummy : in out Boolean);
      --  The recursive routine used to add linker options

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add (Proj : Project_Id; Dummy : in out Boolean) is
         pragma Unreferenced (Dummy);

         Linker_Package : Package_Id;
         Options        : Variable_Value;

      begin
         Linker_Package :=
           Prj.Util.Value_Of
             (Name        => Name_Linker,
              In_Packages => Proj.Decl.Packages,
              In_Tree     => In_Tree);

         Options :=
           Prj.Util.Value_Of
             (Name                    => Name_Ada,
              Index                   => 0,
              Attribute_Or_Array_Name => Name_Linker_Options,
              In_Package              => Linker_Package,
              In_Tree                 => In_Tree);

         --  If attribute is present, add the project with
         --  the attribute to table Linker_Opts.

         if Options /= Nil_Variable_Value then
            Linker_Opts.Increment_Last;
            Linker_Opts.Table (Linker_Opts.Last) :=
              (Project => Proj, Options => Options.Values);
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);

      Dummy : Boolean := False;

   --  Start of processing for Linker_Options_Switches

   begin
      Linker_Opts.Init;

      For_All_Projects (Project, Dummy, Imported_First => True);

      Last_Linker_Option := 0;

      for Index in reverse 1 .. Linker_Opts.Last loop
         declare
            Options : String_List_Id;
            Proj    : constant Project_Id :=
                        Linker_Opts.Table (Index).Project;
            Option  : Name_Id;
            Dir_Path : constant String :=
                         Get_Name_String (Proj.Directory.Name);

         begin
            Options := Linker_Opts.Table (Index).Options;
            while Options /= Nil_String loop
               Option := In_Tree.String_Elements.Table (Options).Value;
               Get_Name_String (Option);

               --  Do not consider empty linker options

               if Name_Len /= 0 then
                  Add_Linker_Option (Name_Buffer (1 .. Name_Len));

                  --  Object files and -L switches specified with relative
                  --  paths must be converted to absolute paths.

                  Test_If_Relative_Path
                    (Switch => Linker_Options_Buffer (Last_Linker_Option),
                     Parent => Dir_Path,
                     Including_L_Switch => True);
               end if;

               Options := In_Tree.String_Elements.Table (Options).Next;
            end loop;
         end;
      end loop;

      return Linker_Options_Buffer (1 .. Last_Linker_Option);
   end Linker_Options_Switches;

   -----------
   -- Mains --
   -----------

   package body Mains is

      type File_And_Loc is record
         File_Name : File_Name_Type;
         Location  : Source_Ptr := No_Location;
      end record;

      package Names is new Table.Table
        (Table_Component_Type => File_And_Loc,
         Table_Index_Type     => Integer,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100,
         Table_Name           => "Makeutl.Mains.Names");
      --  The table that stores the mains

      Current : Natural := 0;
      --  The index of the last main retrieved from the table

      --------------
      -- Add_Main --
      --------------

      procedure Add_Main (Name : String) is
      begin
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Name);
         Names.Increment_Last;
         Names.Table (Names.Last) := (Name_Find, No_Location);
      end Add_Main;

      ------------
      -- Delete --
      ------------

      procedure Delete is
      begin
         Names.Set_Last (0);
         Mains.Reset;
      end Delete;

      ------------------
      -- Get_Location --
      ------------------

      function Get_Location return Source_Ptr is
      begin
         if Current in Names.First .. Names.Last then
            return Names.Table (Current).Location;
         else
            return No_Location;
         end if;
      end Get_Location;

      ---------------
      -- Next_Main --
      ---------------

      function Next_Main return String is
      begin
         if Current >= Names.Last then
            return "";
         else
            Current := Current + 1;
            return Get_Name_String (Names.Table (Current).File_Name);
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

      ------------------
      -- Set_Location --
      ------------------

      procedure Set_Location (Location : Source_Ptr) is
      begin
         if Names.Last > 0 then
            Names.Table (Names.Last).Location := Location;
         end if;
      end Set_Location;

      -----------------
      -- Update_Main --
      -----------------

      procedure Update_Main (Name : String) is
      begin
         if Current in Names.First .. Names.Last then
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Name);
            Names.Table (Current).File_Name := Name_Find;
         end if;
      end Update_Main;
   end Mains;

   ----------
   -- Mark --
   ----------

   procedure Mark (Source_File : File_Name_Type; Index : Int := 0) is
   begin
      Marks.Set (K => (File => Source_File, Index => Index), E => True);
   end Mark;

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

   ---------------------------
   -- Test_If_Relative_Path --
   ---------------------------

   procedure Test_If_Relative_Path
     (Switch               : in out String_Access;
      Parent               : String;
      Including_L_Switch   : Boolean := True;
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
                 and then (Sw (2) = 'A'
                            or else Sw (2) = 'I'
                            or else (Including_L_Switch and then Sw (2) = 'L'))
               then
                  Start := 3;

                  if Sw = "-I-" then
                     return;
                  end if;

               elsif Sw'Length >= 4
                 and then (Sw (2 .. 3) = "aL"
                            or else Sw (2 .. 3) = "aO"
                            or else Sw (2 .. 3) = "aI")
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

               --  Because relative path arguments to --RTS= may be relative
               --  to the search directory prefix, those relative path
               --  arguments are converted only when they include directory
               --  information.

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
                               (Sw (1 .. Start - 1) &
                                Parent &
                                Directory_Separator &
                                Sw (Start .. Sw'Last));
                           return;
                        end if;
                     end loop;

                  else
                     Switch :=
                       new String'
                         (Sw (1 .. Start - 1) &
                          Parent &
                          Directory_Separator &
                          Sw (Start .. Sw'Last));
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
   end Test_If_Relative_Path;

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

end Makeutl;
