------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              M A K E U T L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004 Free Software Foundation, Inc.               --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Namet;    use Namet;
with Osint;    use Osint;
with Prj;      use Prj;
with Prj.Ext;
with Prj.Util;
with Snames;   use Snames;
with Table;
with Types;    use Types;

with System.HTable;

package body Makeutl is

   type Mark_Key is record
      File  : File_Name_Type;
      Index : Int;
   end record;
   --  Identify either a mono-unit source (when Index = 0) or a specific unit
   --  in a multi-unit source.

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
   --  A hash table to keep tracks of the marked units.

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

   ----------------------
   -- Delete_All_Marks --
   ----------------------

   procedure Delete_All_Marks is
   begin
      Marks.Reset;
   end Delete_All_Marks;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Mark_Key) return Mark_Num is
   begin
      return Union_Id (Key.File) mod Max_Mask_Num;
   end Hash;

   ----------------------------
   -- Is_External_Assignment --
   ----------------------------

   function Is_External_Assignment (Argv : String) return Boolean is
      Start     : Positive := 3;
      Finish    : Natural := Argv'Last;
      Equal_Pos : Natural;

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

      if Equal_Pos = Start
        or else Equal_Pos >= Finish
      then
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
     (Project  : Project_Id) return String_List
   is
      procedure Recursive_Add_Linker_Options (Proj : Project_Id);
      --  The recursive routine used to add linker options

      ----------------------------------
      -- Recursive_Add_Linker_Options --
      ----------------------------------

      procedure Recursive_Add_Linker_Options (Proj : Project_Id) is
         Data           : Project_Data;
         Linker_Package : Package_Id;
         Options        : Variable_Value;
         Imported       : Project_List;

      begin
         if Proj /= No_Project then
            Data := Projects.Table (Proj);

            if not Data.Seen then
               Projects.Table (Proj).Seen := True;
               Imported := Data.Imported_Projects;

               while Imported /= Empty_Project_List loop
                  Recursive_Add_Linker_Options
                    (Project_Lists.Table (Imported).Project);
                  Imported := Project_Lists.Table (Imported).Next;
               end loop;

               if Proj /= Project then
                  Linker_Package :=
                    Prj.Util.Value_Of
                      (Name => Name_Linker,
                       In_Packages => Data.Decl.Packages);
                  Options :=
                    Prj.Util.Value_Of
                      (Name => Name_Ada,
                       Index => 0,
                       Attribute_Or_Array_Name => Name_Linker_Options,
                       In_Package => Linker_Package);

                  --  If attribute is present, add the project with
                  --  the attribute to table Linker_Opts.

                  if Options /= Nil_Variable_Value then
                     Linker_Opts.Increment_Last;
                     Linker_Opts.Table (Linker_Opts.Last) :=
                       (Project => Proj, Options => Options.Values);
                  end if;
               end if;
            end if;
         end if;
      end Recursive_Add_Linker_Options;

   --  Start of processing for Linker_Options_Switches

   begin
      Linker_Opts.Init;

      for Index in 1 .. Projects.Last loop
         Projects.Table (Index).Seen := False;
      end loop;

      Recursive_Add_Linker_Options (Project);

      Last_Linker_Option := 0;

      for Index in reverse 1 .. Linker_Opts.Last loop
         declare
            Options : String_List_Id := Linker_Opts.Table (Index).Options;
            Proj    : constant Project_Id :=
              Linker_Opts.Table (Index).Project;
            Option  : Name_Id;

         begin
            --  If Dir_Path has not been computed for this project, do it now

            if Projects.Table (Proj).Dir_Path = null then
               Projects.Table (Proj).Dir_Path :=
                 new String'
                   (Get_Name_String (Projects.Table (Proj). Directory));
            end if;

            while Options /= Nil_String loop
               Option := String_Elements.Table (Options).Value;
               Options := String_Elements.Table (Options).Next;
               Add_Linker_Option (Get_Name_String (Option));

               --  Object files and -L switches specified with
               --  relative paths and must be converted to
               --  absolute paths.

               Test_If_Relative_Path
                 (Switch =>
                    Linker_Options_Buffer (Last_Linker_Option),
                  Parent => Projects.Table (Proj).Dir_Path,
                  Including_L_Switch => True);
            end loop;
         end;
      end loop;

      return Linker_Options_Buffer (1 .. Last_Linker_Option);
   end Linker_Options_Switches;

   -----------
   -- Mains --
   -----------

   package body Mains is

      package Names is new Table.Table
        (Table_Component_Type => File_Name_Type,
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
         Names.Table (Names.Last) := Name_Find;
      end Add_Main;

      ------------
      -- Delete --
      ------------

      procedure Delete is
      begin
         Names.Set_Last (0);
         Reset;
      end Delete;

      ---------------
      -- Next_Main --
      ---------------

      function Next_Main return String is
      begin
         if Current >= Names.Last then
            return "";

         else
            Current := Current + 1;
            return Get_Name_String (Names.Table (Current));
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

   end Mains;

   ----------
   -- Mark --
   ----------

   procedure Mark (Source_File : File_Name_Type; Index : Int := 0) is
   begin
      Marks.Set (K => (File => Source_File, Index => Index), E => True);
   end Mark;

   ---------------------------
   -- Test_If_Relative_Path --
   ---------------------------

   procedure Test_If_Relative_Path
     (Switch             : in out String_Access;
      Parent             : String_Access;
      Including_L_Switch : Boolean := True)
   is
   begin
      if Switch /= null then
         declare
            Sw : String (1 .. Switch'Length);
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

               else
                  return;
               end if;

               --  Because relative path arguments to --RTS= may be relative
               --  to the search directory prefix, those relative path
               --  arguments are not converted.

               if not Is_Absolute_Path (Sw (Start .. Sw'Last)) then
                  if Parent = null or else Parent'Length = 0 then
                     Do_Fail
                       ("relative search path switches (""",
                        Sw,
                        """) are not allowed");

                  else
                     Switch :=
                       new String'
                         (Sw (1 .. Start - 1) &
                          Parent.all &
                          Directory_Separator &
                          Sw (Start .. Sw'Last));
                  end if;
               end if;

            else
               if not Is_Absolute_Path (Sw) then
                  if Parent = null or else Parent'Length = 0 then
                     Do_Fail
                       ("relative paths (""", Sw, """) are not allowed");

                  else
                     Switch :=
                       new String'(Parent.all & Directory_Separator & Sw);
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

      --  If there is no difits, or if the digits are not preceded by
      --  the character that precedes a unit index, this is not the ALI file
      --  of a unit in a multi-unit source.

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
