------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . C O M M A N D _ L I N E                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2009, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib; use GNAT.OS_Lib;

package body GNAT.Command_Line is

   package CL renames Ada.Command_Line;

   type Switch_Parameter_Type is
     (Parameter_None,
      Parameter_With_Optional_Space,  --  ':' in getopt
      Parameter_With_Space_Or_Equal,  --  '=' in getopt
      Parameter_No_Space,             --  '!' in getopt
      Parameter_Optional);            --  '?' in getopt

   procedure Set_Parameter
     (Variable : out Parameter_Type;
      Arg_Num  : Positive;
      First    : Positive;
      Last     : Positive;
      Extra    : Character := ASCII.NUL);
   pragma Inline (Set_Parameter);
   --  Set the parameter that will be returned by Parameter below
   --  Parameters need to be defined ???

   function Goto_Next_Argument_In_Section (Parser : Opt_Parser) return Boolean;
   --  Go to the next argument on the command line. If we are at the end of
   --  the current section, we want to make sure there is no other identical
   --  section on the command line (there might be multiple instances of
   --  -largs). Returns True iff there is another argument.

   function Get_File_Names_Case_Sensitive return Integer;
   pragma Import (C, Get_File_Names_Case_Sensitive,
                  "__gnat_get_file_names_case_sensitive");

   File_Names_Case_Sensitive : constant Boolean :=
                                 Get_File_Names_Case_Sensitive /= 0;

   procedure Canonical_Case_File_Name (S : in out String);
   --  Given a file name, converts it to canonical case form. For systems where
   --  file names are case sensitive, this procedure has no effect. If file
   --  names are not case sensitive (i.e. for example if you have the file
   --  "xyz.adb", you can refer to it as XYZ.adb or XyZ.AdB), then this call
   --  converts the given string to canonical all lower case form, so that two
   --  file names compare equal if they refer to the same file.

   procedure Internal_Initialize_Option_Scan
     (Parser                   : Opt_Parser;
      Switch_Char              : Character;
      Stop_At_First_Non_Switch : Boolean;
      Section_Delimiters       : String);
   --  Initialize Parser, which must have been allocated already

   function Argument (Parser : Opt_Parser; Index : Integer) return String;
   --  Return the index-th command line argument

   procedure Find_Longest_Matching_Switch
     (Switches          : String;
      Arg               : String;
      Index_In_Switches : out Integer;
      Switch_Length     : out Integer;
      Param             : out Switch_Parameter_Type);
   --  return the Longest switch from Switches that matches at least
   --  partially Arg. Index_In_Switches is set to 0 if none matches

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Command_Line_Configuration_Record, Command_Line_Configuration);

   procedure Remove (Line : in out Argument_List_Access; Index : Integer);
   --  Remove a specific element from Line

   procedure Add
     (Line   : in out Argument_List_Access;
      Str    : String_Access;
      Before : Boolean := False);
   --  Add a new element to Line. If Before is True, the item is inserted at
   --  the beginning, else it is appended.

   function Can_Have_Parameter (S : String) return Boolean;
   --  True if S can have a parameter.

   function Require_Parameter (S : String) return Boolean;
   --  True if S requires a parameter.

   function Actual_Switch (S : String) return String;
   --  Remove any possible trailing '!', ':', '?' and '='

   generic
      with procedure Callback (Simple_Switch : String; Parameter : String);
   procedure For_Each_Simple_Switch
     (Cmd       : Command_Line;
      Switch    : String;
      Parameter : String  := "";
      Unalias   : Boolean := True);
   --  Breaks Switch into as simple switches as possible (expanding aliases and
   --  ungrouping common prefixes when possible), and call Callback for each of
   --  these.

   procedure Sort_Sections
     (Line     : GNAT.OS_Lib.Argument_List_Access;
      Sections : GNAT.OS_Lib.Argument_List_Access;
      Params   : GNAT.OS_Lib.Argument_List_Access);
   --  Reorder the command line switches so that the switches belonging to a
   --  section are grouped together.

   procedure Group_Switches
     (Cmd      : Command_Line;
      Result   : Argument_List_Access;
      Sections : Argument_List_Access;
      Params   : Argument_List_Access);
   --  Group switches with common prefixes whenever possible. Once they have
   --  been grouped, we also check items for possible aliasing.

   procedure Alias_Switches
     (Cmd    : Command_Line;
      Result : Argument_List_Access;
      Params : Argument_List_Access);
   --  When possible, replace one or more switches by an alias, i.e. a shorter
   --  version.

   function Looking_At
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Boolean;
   --  Return True if the characters starting at Index in Type_Str are
   --  equivalent to Substring.

   --------------
   -- Argument --
   --------------

   function Argument (Parser : Opt_Parser; Index : Integer) return String is
   begin
      if Parser.Arguments /= null then
         return Parser.Arguments (Index + Parser.Arguments'First - 1).all;
      else
         return CL.Argument (Index);
      end if;
   end Argument;

   ------------------------------
   -- Canonical_Case_File_Name --
   ------------------------------

   procedure Canonical_Case_File_Name (S : in out String) is
   begin
      if not File_Names_Case_Sensitive then
         for J in S'Range loop
            if S (J) in 'A' .. 'Z' then
               S (J) := Character'Val
                         (Character'Pos (S (J)) +
                          Character'Pos ('a')   -
                          Character'Pos ('A'));
            end if;
         end loop;
      end if;
   end Canonical_Case_File_Name;

   ---------------
   -- Expansion --
   ---------------

   function Expansion (Iterator : Expansion_Iterator) return String is
      use GNAT.Directory_Operations;
      type Pointer is access all Expansion_Iterator;

      It   : constant Pointer := Iterator'Unrestricted_Access;
      S    : String (1 .. 1024);
      Last : Natural;

      Current : Depth := It.Current_Depth;
      NL      : Positive;

   begin
      --  It is assumed that a directory is opened at the current level.
      --  Otherwise GNAT.Directory_Operations.Directory_Error will be raised
      --  at the first call to Read.

      loop
         Read (It.Levels (Current).Dir, S, Last);

         --  If we have exhausted the directory, close it and go back one level

         if Last = 0 then
            Close (It.Levels (Current).Dir);

            --  If we are at level 1, we are finished; return an empty string

            if Current = 1 then
               return String'(1 .. 0 => ' ');
            else
               --  Otherwise continue with the directory at the previous level

               Current := Current - 1;
               It.Current_Depth := Current;
            end if;

         --  If this is a directory, that is neither "." or "..", attempt to
         --  go to the next level.

         elsif Is_Directory
           (It.Dir_Name (1 .. It.Levels (Current).Name_Last) & S (1 .. Last))
           and then S (1 .. Last) /= "."
           and then S (1 .. Last) /= ".."
         then
            --  We can go to the next level only if we have not reached the
            --  maximum depth,

            if Current < It.Maximum_Depth then
               NL := It.Levels (Current).Name_Last;

               --  And if relative path of this new directory is not too long

               if NL + Last + 1 < Max_Path_Length then
                  Current := Current + 1;
                  It.Current_Depth := Current;
                  It.Dir_Name (NL + 1 .. NL + Last) := S (1 .. Last);
                  NL := NL + Last + 1;
                  It.Dir_Name (NL) := Directory_Separator;
                  It.Levels (Current).Name_Last := NL;
                  Canonical_Case_File_Name (It.Dir_Name (1 .. NL));

                  --  Open the new directory, and read from it

                  GNAT.Directory_Operations.Open
                    (It.Levels (Current).Dir, It.Dir_Name (1 .. NL));
               end if;
            end if;

         --  If not a directory, check the relative path against the pattern

         else
            declare
               Name : String :=
                        It.Dir_Name (It.Start .. It.Levels (Current).Name_Last)
                          & S (1 .. Last);
            begin
               Canonical_Case_File_Name (Name);

               --  If it matches return the relative path

               if GNAT.Regexp.Match (Name, Iterator.Regexp) then
                  return Name;
               end if;
            end;
         end if;
      end loop;
   end Expansion;

   -----------------
   -- Full_Switch --
   -----------------

   function Full_Switch
     (Parser : Opt_Parser := Command_Line_Parser) return String
   is
   begin
      if Parser.The_Switch.Extra = ASCII.NUL then
         return Argument (Parser, Parser.The_Switch.Arg_Num)
           (Parser.The_Switch.First .. Parser.The_Switch.Last);
      else
         return Parser.The_Switch.Extra
           & Argument (Parser, Parser.The_Switch.Arg_Num)
           (Parser.The_Switch.First .. Parser.The_Switch.Last);
      end if;
   end Full_Switch;

   ------------------
   -- Get_Argument --
   ------------------

   function Get_Argument
     (Do_Expansion : Boolean    := False;
      Parser       : Opt_Parser := Command_Line_Parser) return String
   is
   begin
      if Parser.In_Expansion then
         declare
            S : constant String := Expansion (Parser.Expansion_It);
         begin
            if S'Length /= 0 then
               return S;
            else
               Parser.In_Expansion := False;
            end if;
         end;
      end if;

      if Parser.Current_Argument > Parser.Arg_Count then

         --  If this is the first time this function is called

         if Parser.Current_Index = 1 then
            Parser.Current_Argument := 1;
            while Parser.Current_Argument <= Parser.Arg_Count
              and then Parser.Section (Parser.Current_Argument) /=
                Parser.Current_Section
            loop
               Parser.Current_Argument := Parser.Current_Argument + 1;
            end loop;
         else
            return String'(1 .. 0 => ' ');
         end if;

      elsif Parser.Section (Parser.Current_Argument) = 0 then
         while Parser.Current_Argument <= Parser.Arg_Count
           and then Parser.Section (Parser.Current_Argument) /=
             Parser.Current_Section
         loop
            Parser.Current_Argument := Parser.Current_Argument + 1;
         end loop;
      end if;

      Parser.Current_Index := Integer'Last;

      while Parser.Current_Argument <= Parser.Arg_Count
        and then Parser.Is_Switch (Parser.Current_Argument)
      loop
         Parser.Current_Argument := Parser.Current_Argument + 1;
      end loop;

      if Parser.Current_Argument > Parser.Arg_Count then
         return String'(1 .. 0 => ' ');
      elsif Parser.Section (Parser.Current_Argument) = 0 then
         return Get_Argument (Do_Expansion);
      end if;

      Parser.Current_Argument := Parser.Current_Argument + 1;

      --  Could it be a file name with wild cards to expand?

      if Do_Expansion then
         declare
            Arg   : constant String :=
                      Argument (Parser, Parser.Current_Argument - 1);
            Index : Positive;

         begin
            Index := Arg'First;
            while Index <= Arg'Last loop
               if Arg (Index) = '*'
                 or else Arg (Index) = '?'
                 or else Arg (Index) = '['
               then
                  Parser.In_Expansion := True;
                  Start_Expansion (Parser.Expansion_It, Arg);
                  return Get_Argument (Do_Expansion);
               end if;

               Index := Index + 1;
            end loop;
         end;
      end if;

      return Argument (Parser, Parser.Current_Argument - 1);
   end Get_Argument;

   ----------------------------------
   -- Find_Longest_Matching_Switch --
   ----------------------------------

   procedure Find_Longest_Matching_Switch
     (Switches          : String;
      Arg               : String;
      Index_In_Switches : out Integer;
      Switch_Length     : out Integer;
      Param             : out Switch_Parameter_Type)
   is
      Index  : Natural;
      Length : Natural := 1;
      P      : Switch_Parameter_Type;

   begin
      Index_In_Switches := 0;
      Switch_Length     := 0;

      --  Remove all leading spaces first to make sure that Index points
      --  at the start of the first switch.

      Index := Switches'First;
      while Index <= Switches'Last and then Switches (Index) = ' ' loop
         Index := Index + 1;
      end loop;

      while Index <= Switches'Last loop

         --  Search the length of the parameter at this position in Switches

         Length := Index;
         while Length <= Switches'Last
           and then Switches (Length) /= ' '
         loop
            Length := Length + 1;
         end loop;

         if Length = Index + 1 then
            P := Parameter_None;
         else
            case Switches (Length - 1) is
               when ':'    =>
                  P      := Parameter_With_Optional_Space;
                  Length := Length - 1;
               when '='    =>
                  P      := Parameter_With_Space_Or_Equal;
                  Length := Length - 1;
               when '!'    =>
                  P      := Parameter_No_Space;
                  Length := Length - 1;
               when '?'    =>
                  P      := Parameter_Optional;
                  Length := Length - 1;
               when others =>
                  P      := Parameter_None;
            end case;
         end if;

         --  If it is the one we searched, it may be a candidate

         if Arg'First + Length - 1 - Index <= Arg'Last
           and then Switches (Index .. Length - 1) =
                      Arg (Arg'First .. Arg'First + Length - 1 - Index)
           and then Length - Index > Switch_Length
         then
            Param             := P;
            Index_In_Switches := Index;
            Switch_Length     := Length - Index;
         end if;

         --  Look for the next switch in Switches

         while Index <= Switches'Last
           and then Switches (Index) /= ' '
         loop
            Index := Index + 1;
         end loop;

         Index := Index + 1;
      end loop;
   end Find_Longest_Matching_Switch;

   ------------
   -- Getopt --
   ------------

   function Getopt
     (Switches    : String;
      Concatenate : Boolean := True;
      Parser      : Opt_Parser := Command_Line_Parser) return Character
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);

   begin
      <<Restart>>

      --  If we have finished parsing the current command line item (there
      --  might be multiple switches in a single item), then go to the next
      --  element

      if Parser.Current_Argument > Parser.Arg_Count
        or else (Parser.Current_Index >
                   Argument (Parser, Parser.Current_Argument)'Last
                 and then not Goto_Next_Argument_In_Section (Parser))
      then
         return ASCII.NUL;
      end if;

      --  By default, the switch will not have a parameter

      Parser.The_Parameter :=
        (Integer'Last, Integer'Last, Integer'Last - 1, ASCII.NUL);
      Parser.The_Separator := ASCII.NUL;

      declare
         Arg            : constant String :=
                            Argument (Parser, Parser.Current_Argument);
         Index_Switches : Natural := 0;
         Max_Length     : Natural := 0;
         End_Index      : Natural;
         Param          : Switch_Parameter_Type;
      begin
         --  If we are on a new item, test if this might be a switch

         if Parser.Current_Index = Arg'First then
            if Arg (Arg'First) /= Parser.Switch_Character then

               --  If it isn't a switch, return it immediately. We also know it
               --  isn't the parameter to a previous switch, since that has
               --  already been handled

               if Switches (Switches'First) = '*' then
                  Set_Parameter
                    (Parser.The_Switch,
                     Arg_Num => Parser.Current_Argument,
                     First   => Arg'First,
                     Last    => Arg'Last);
                  Parser.Is_Switch (Parser.Current_Argument) := True;
                  Dummy := Goto_Next_Argument_In_Section (Parser);
                  return '*';
               end if;

               if Parser.Stop_At_First then
                  Parser.Current_Argument := Positive'Last;
                  return ASCII.NUL;

               elsif not Goto_Next_Argument_In_Section (Parser) then
                  return ASCII.NUL;

               else
                  --  Recurse to get the next switch on the command line

                  goto Restart;
               end if;
            end if;

            --  We are on the first character of a new command line argument,
            --  which starts with Switch_Character. Further analysis is needed.

            Parser.Current_Index := Parser.Current_Index + 1;
            Parser.Is_Switch (Parser.Current_Argument) := True;
         end if;

         Find_Longest_Matching_Switch
           (Switches          => Switches,
            Arg               => Arg (Parser.Current_Index .. Arg'Last),
            Index_In_Switches => Index_Switches,
            Switch_Length     => Max_Length,
            Param             => Param);

         --  If switch is not accepted, it is either invalid or is returned
         --  in the context of '*'.

         if Index_Switches = 0 then

            --  Depending on the value of Concatenate, the full switch is
            --  a single character or the rest of the argument.

            if Concatenate then
               End_Index := Parser.Current_Index;
            else
               End_Index := Arg'Last;
            end if;

            if Switches (Switches'First) = '*' then

               --  Always prepend the switch character, so that users know that
               --  this comes from a switch on the command line. This is
               --  especially important when Concatenate is False, since
               --  otherwise the current argument first character is lost.

               Set_Parameter
                 (Parser.The_Switch,
                  Arg_Num => Parser.Current_Argument,
                  First   => Parser.Current_Index,
                  Last    => Arg'Last,
                  Extra   => Parser.Switch_Character);
               Parser.Is_Switch (Parser.Current_Argument) := True;
               Dummy := Goto_Next_Argument_In_Section (Parser);
               return '*';
            end if;

            Set_Parameter
              (Parser.The_Switch,
               Arg_Num => Parser.Current_Argument,
               First   => Parser.Current_Index,
               Last    => End_Index);
            Parser.Current_Index := End_Index + 1;
            raise Invalid_Switch;
         end if;

         End_Index := Parser.Current_Index + Max_Length - 1;
         Set_Parameter
           (Parser.The_Switch,
            Arg_Num => Parser.Current_Argument,
            First   => Parser.Current_Index,
            Last    => End_Index);

         case Param is
            when Parameter_With_Optional_Space =>
               if End_Index < Arg'Last then
                  Set_Parameter
                    (Parser.The_Parameter,
                     Arg_Num => Parser.Current_Argument,
                     First   => End_Index + 1,
                     Last    => Arg'Last);
                  Dummy := Goto_Next_Argument_In_Section (Parser);

               elsif Parser.Current_Argument < Parser.Arg_Count
                 and then Parser.Section (Parser.Current_Argument + 1) /= 0
               then
                  Parser.Current_Argument := Parser.Current_Argument + 1;
                  Parser.The_Separator := ' ';
                  Set_Parameter
                    (Parser.The_Parameter,
                     Arg_Num => Parser.Current_Argument,
                     First => Argument (Parser, Parser.Current_Argument)'First,
                     Last  => Argument (Parser, Parser.Current_Argument)'Last);
                  Parser.Is_Switch (Parser.Current_Argument) := True;
                  Dummy := Goto_Next_Argument_In_Section (Parser);

               else
                  Parser.Current_Index := End_Index + 1;
                  raise Invalid_Parameter;
               end if;

            when Parameter_With_Space_Or_Equal =>

               --  If the switch is of the form <switch>=xxx

               if End_Index < Arg'Last then

                  if Arg (End_Index + 1) = '='
                    and then End_Index + 1 < Arg'Last
                  then
                     Parser.The_Separator := '=';
                     Set_Parameter
                       (Parser.The_Parameter,
                        Arg_Num => Parser.Current_Argument,
                        First   => End_Index + 2,
                        Last    => Arg'Last);
                     Dummy := Goto_Next_Argument_In_Section (Parser);
                  else
                     Parser.Current_Index := End_Index + 1;
                     raise Invalid_Parameter;
                  end if;

               --  If the switch is of the form <switch> xxx

               elsif Parser.Current_Argument < Parser.Arg_Count
                 and then Parser.Section (Parser.Current_Argument + 1) /= 0
               then
                  Parser.Current_Argument := Parser.Current_Argument + 1;
                  Parser.The_Separator := ' ';
                  Set_Parameter
                    (Parser.The_Parameter,
                     Arg_Num => Parser.Current_Argument,
                     First => Argument (Parser, Parser.Current_Argument)'First,
                     Last  => Argument (Parser, Parser.Current_Argument)'Last);
                  Parser.Is_Switch (Parser.Current_Argument) := True;
                  Dummy := Goto_Next_Argument_In_Section (Parser);

               else
                  Parser.Current_Index := End_Index + 1;
                  raise Invalid_Parameter;
               end if;

            when Parameter_No_Space =>

               if End_Index < Arg'Last then
                  Set_Parameter
                    (Parser.The_Parameter,
                     Arg_Num => Parser.Current_Argument,
                     First   => End_Index + 1,
                     Last    => Arg'Last);
                  Dummy := Goto_Next_Argument_In_Section (Parser);

               else
                  Parser.Current_Index := End_Index + 1;
                  raise Invalid_Parameter;
               end if;

            when Parameter_Optional =>

               if End_Index < Arg'Last then
                  Set_Parameter
                    (Parser.The_Parameter,
                     Arg_Num => Parser.Current_Argument,
                     First   => End_Index + 1,
                     Last    => Arg'Last);
               end if;

               Dummy := Goto_Next_Argument_In_Section (Parser);

            when Parameter_None =>

               if Concatenate or else End_Index = Arg'Last then
                  Parser.Current_Index := End_Index + 1;

               else
                  --  If Concatenate is False and the full argument is not
                  --  recognized as a switch, this is an invalid switch.

                  if Switches (Switches'First) = '*' then
                     Set_Parameter
                       (Parser.The_Switch,
                        Arg_Num => Parser.Current_Argument,
                        First   => Arg'First,
                        Last    => Arg'Last);
                     Parser.Is_Switch (Parser.Current_Argument) := True;
                     Dummy := Goto_Next_Argument_In_Section (Parser);
                     return '*';
                  end if;

                  Set_Parameter
                    (Parser.The_Switch,
                     Arg_Num => Parser.Current_Argument,
                     First   => Parser.Current_Index,
                     Last    => Arg'Last);
                  Parser.Current_Index := Arg'Last + 1;
                  raise Invalid_Switch;
               end if;
         end case;

         return Switches (Index_Switches);
      end;
   end Getopt;

   -----------------------------------
   -- Goto_Next_Argument_In_Section --
   -----------------------------------

   function Goto_Next_Argument_In_Section
     (Parser : Opt_Parser) return Boolean
   is
   begin
      Parser.Current_Argument := Parser.Current_Argument + 1;

      if Parser.Current_Argument > Parser.Arg_Count
        or else Parser.Section (Parser.Current_Argument) = 0
      then
         loop
            Parser.Current_Argument := Parser.Current_Argument + 1;

            if Parser.Current_Argument > Parser.Arg_Count then
               Parser.Current_Index := 1;
               return False;
            end if;

            exit when Parser.Section (Parser.Current_Argument) =
                                                  Parser.Current_Section;
         end loop;
      end if;

      Parser.Current_Index :=
        Argument (Parser, Parser.Current_Argument)'First;

      return True;
   end Goto_Next_Argument_In_Section;

   ------------------
   -- Goto_Section --
   ------------------

   procedure Goto_Section
     (Name   : String := "";
      Parser : Opt_Parser := Command_Line_Parser)
   is
      Index : Integer;

   begin
      Parser.In_Expansion := False;

      if Name = "" then
         Parser.Current_Argument := 1;
         Parser.Current_Index    := 1;
         Parser.Current_Section  := 1;
         return;
      end if;

      Index := 1;
      while Index <= Parser.Arg_Count loop
         if Parser.Section (Index) = 0
           and then Argument (Parser, Index) = Parser.Switch_Character & Name
         then
            Parser.Current_Argument := Index + 1;
            Parser.Current_Index    := 1;

            if Parser.Current_Argument <= Parser.Arg_Count then
               Parser.Current_Section :=
                 Parser.Section (Parser.Current_Argument);
            end if;
            return;
         end if;

         Index := Index + 1;
      end loop;

      Parser.Current_Argument := Positive'Last;
      Parser.Current_Index := 2;   --  so that Get_Argument returns nothing
   end Goto_Section;

   ----------------------------
   -- Initialize_Option_Scan --
   ----------------------------

   procedure Initialize_Option_Scan
     (Switch_Char              : Character := '-';
      Stop_At_First_Non_Switch : Boolean   := False;
      Section_Delimiters       : String    := "")
   is
   begin
      Internal_Initialize_Option_Scan
        (Parser                   => Command_Line_Parser,
         Switch_Char              => Switch_Char,
         Stop_At_First_Non_Switch => Stop_At_First_Non_Switch,
         Section_Delimiters       => Section_Delimiters);
   end Initialize_Option_Scan;

   ----------------------------
   -- Initialize_Option_Scan --
   ----------------------------

   procedure Initialize_Option_Scan
     (Parser                   : out Opt_Parser;
      Command_Line             : GNAT.OS_Lib.Argument_List_Access;
      Switch_Char              : Character := '-';
      Stop_At_First_Non_Switch : Boolean := False;
      Section_Delimiters       : String := "")
   is
   begin
      Free (Parser);

      if Command_Line = null then
         Parser := new Opt_Parser_Data (CL.Argument_Count);
         Initialize_Option_Scan
           (Switch_Char              => Switch_Char,
            Stop_At_First_Non_Switch => Stop_At_First_Non_Switch,
            Section_Delimiters       => Section_Delimiters);
      else
         Parser := new Opt_Parser_Data (Command_Line'Length);
         Parser.Arguments := Command_Line;
         Internal_Initialize_Option_Scan
           (Parser                   => Parser,
            Switch_Char              => Switch_Char,
            Stop_At_First_Non_Switch => Stop_At_First_Non_Switch,
            Section_Delimiters       => Section_Delimiters);
      end if;
   end Initialize_Option_Scan;

   -------------------------------------
   -- Internal_Initialize_Option_Scan --
   -------------------------------------

   procedure Internal_Initialize_Option_Scan
     (Parser                   : Opt_Parser;
      Switch_Char              : Character;
      Stop_At_First_Non_Switch : Boolean;
      Section_Delimiters       : String)
   is
      Section_Num     : Section_Number;
      Section_Index   : Integer;
      Last            : Integer;
      Delimiter_Found : Boolean;

      Discard : Boolean;
      pragma Warnings (Off, Discard);

   begin
      Parser.Current_Argument := 0;
      Parser.Current_Index    := 0;
      Parser.In_Expansion     := False;
      Parser.Switch_Character := Switch_Char;
      Parser.Stop_At_First    := Stop_At_First_Non_Switch;

      --  If we are using sections, we have to preprocess the command line
      --  to delimit them. A section can be repeated, so we just give each
      --  item on the command line a section number

      Section_Num   := 1;
      Section_Index := Section_Delimiters'First;
      while Section_Index <= Section_Delimiters'Last loop
         Last := Section_Index;
         while Last <= Section_Delimiters'Last
           and then Section_Delimiters (Last) /= ' '
         loop
            Last := Last + 1;
         end loop;

         Delimiter_Found := False;
         Section_Num := Section_Num + 1;

         for Index in 1 .. Parser.Arg_Count loop
            if Argument (Parser, Index)(1) = Parser.Switch_Character
              and then
                Argument (Parser, Index) = Parser.Switch_Character &
                                        Section_Delimiters
                                          (Section_Index .. Last - 1)
            then
               Parser.Section (Index) := 0;
               Delimiter_Found := True;

            elsif Parser.Section (Index) = 0 then
               Delimiter_Found := False;

            elsif Delimiter_Found then
               Parser.Section (Index) := Section_Num;
            end if;
         end loop;

         Section_Index := Last + 1;
         while Section_Index <= Section_Delimiters'Last
           and then Section_Delimiters (Section_Index) = ' '
         loop
            Section_Index := Section_Index + 1;
         end loop;
      end loop;

      Discard := Goto_Next_Argument_In_Section (Parser);
   end Internal_Initialize_Option_Scan;

   ---------------
   -- Parameter --
   ---------------

   function Parameter
     (Parser : Opt_Parser := Command_Line_Parser) return String
   is
   begin
      if Parser.The_Parameter.First > Parser.The_Parameter.Last then
         return String'(1 .. 0 => ' ');
      else
         return Argument (Parser, Parser.The_Parameter.Arg_Num)
           (Parser.The_Parameter.First .. Parser.The_Parameter.Last);
      end if;
   end Parameter;

   ---------------
   -- Separator --
   ---------------

   function Separator
     (Parser : Opt_Parser := Command_Line_Parser) return Character
   is
   begin
      return Parser.The_Separator;
   end Separator;

   -------------------
   -- Set_Parameter --
   -------------------

   procedure Set_Parameter
     (Variable : out Parameter_Type;
      Arg_Num  : Positive;
      First    : Positive;
      Last     : Positive;
      Extra    : Character := ASCII.NUL)
   is
   begin
      Variable.Arg_Num := Arg_Num;
      Variable.First   := First;
      Variable.Last    := Last;
      Variable.Extra   := Extra;
   end Set_Parameter;

   ---------------------
   -- Start_Expansion --
   ---------------------

   procedure Start_Expansion
     (Iterator     : out Expansion_Iterator;
      Pattern      : String;
      Directory    : String := "";
      Basic_Regexp : Boolean := True)
   is
      Directory_Separator : Character;
      pragma Import (C, Directory_Separator, "__gnat_dir_separator");

      First : Positive := Pattern'First;
      Pat   : String := Pattern;

   begin
      Canonical_Case_File_Name (Pat);
      Iterator.Current_Depth := 1;

      --  If Directory is unspecified, use the current directory ("./" or ".\")

      if Directory = "" then
         Iterator.Dir_Name (1 .. 2) := "." & Directory_Separator;
         Iterator.Start := 3;

      else
         Iterator.Dir_Name (1 .. Directory'Length) := Directory;
         Iterator.Start := Directory'Length + 1;
         Canonical_Case_File_Name (Iterator.Dir_Name (1 .. Directory'Length));

         --  Make sure that the last character is a directory separator

         if Directory (Directory'Last) /= Directory_Separator then
            Iterator.Dir_Name (Iterator.Start) := Directory_Separator;
            Iterator.Start := Iterator.Start + 1;
         end if;
      end if;

      Iterator.Levels (1).Name_Last := Iterator.Start - 1;

      --  Open the initial Directory, at depth 1

      GNAT.Directory_Operations.Open
        (Iterator.Levels (1).Dir, Iterator.Dir_Name (1 .. Iterator.Start - 1));

      --  If in the current directory and the pattern starts with "./" or ".\",
      --  drop the "./" or ".\" from the pattern.

      if Directory = "" and then Pat'Length > 2
        and then Pat (Pat'First) = '.'
        and then Pat (Pat'First + 1) = Directory_Separator
      then
         First := Pat'First + 2;
      end if;

      Iterator.Regexp :=
        GNAT.Regexp.Compile (Pat (First .. Pat'Last), Basic_Regexp, True);

      Iterator.Maximum_Depth := 1;

      --  Maximum_Depth is equal to 1 plus the number of directory separators
      --  in the pattern.

      for Index in First .. Pat'Last loop
         if Pat (Index) = Directory_Separator then
            Iterator.Maximum_Depth := Iterator.Maximum_Depth + 1;
            exit when Iterator.Maximum_Depth = Max_Depth;
         end if;
      end loop;
   end Start_Expansion;

   ----------
   -- Free --
   ----------

   procedure Free (Parser : in out Opt_Parser) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Opt_Parser_Data, Opt_Parser);
   begin
      if Parser /= null
        and then Parser /= Command_Line_Parser
      then
         Free (Parser.Arguments);
         Unchecked_Free (Parser);
      end if;
   end Free;

   ------------------
   -- Define_Alias --
   ------------------

   procedure Define_Alias
     (Config   : in out Command_Line_Configuration;
      Switch   : String;
      Expanded : String)
   is
   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Add (Config.Aliases,    new String'(Switch));
      Add (Config.Expansions, new String'(Expanded));
   end Define_Alias;

   -------------------
   -- Define_Prefix --
   -------------------

   procedure Define_Prefix
     (Config : in out Command_Line_Configuration;
      Prefix : String)
   is
   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Add (Config.Prefixes, new String'(Prefix));
   end Define_Prefix;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config : in out Command_Line_Configuration;
      Switch : String)
   is
   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Add (Config.Switches, new String'(Switch));
   end Define_Switch;

   --------------------
   -- Define_Section --
   --------------------

   procedure Define_Section
     (Config : in out Command_Line_Configuration;
      Section : String)
   is
   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Add (Config.Sections, new String'(Section));
   end Define_Section;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Config      : Command_Line_Configuration;
      Switch_Char : Character)
      return String
   is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
      use type Ada.Strings.Unbounded.Unbounded_String;

   begin
      if Config = null or else Config.Switches = null then
         return "";
      end if;

      for J in Config.Switches'Range loop
         if Config.Switches (J) (Config.Switches (J)'First) = Switch_Char then
            Ret :=
              Ret & " " &
                Config.Switches (J)
                  (Config.Switches (J)'First + 1 .. Config.Switches (J)'Last);
         else
            Ret := Ret & " " & Config.Switches (J).all;
         end if;
      end loop;

      return Ada.Strings.Unbounded.To_String (Ret);
   end Get_Switches;

   -----------------------
   -- Set_Configuration --
   -----------------------

   procedure Set_Configuration
     (Cmd    : in out Command_Line;
      Config : Command_Line_Configuration)
   is
   begin
      Cmd.Config := Config;
   end Set_Configuration;

   -----------------------
   -- Get_Configuration --
   -----------------------

   function Get_Configuration
     (Cmd : Command_Line) return Command_Line_Configuration is
   begin
      return Cmd.Config;
   end Get_Configuration;

   ----------------------
   -- Set_Command_Line --
   ----------------------

   procedure Set_Command_Line
     (Cmd                : in out Command_Line;
      Switches           : String;
      Getopt_Description : String := "";
      Switch_Char        : Character := '-')
   is
      Tmp     : Argument_List_Access;
      Parser  : Opt_Parser;
      S       : Character;
      Section : String_Access := null;

      function Real_Full_Switch
        (S      : Character;
         Parser : Opt_Parser) return String;
      --  Ensure that the returned switch value contains the
      --  Switch_Char prefix if needed.

      ----------------------
      -- Real_Full_Switch --
      ----------------------

      function Real_Full_Switch
        (S      : Character;
         Parser : Opt_Parser) return String
      is
      begin
         if S = '*' then
            return Full_Switch (Parser);
         else
            return Switch_Char & Full_Switch (Parser);
         end if;
      end Real_Full_Switch;

   --  Start of processing for Set_Command_Line

   begin
      Free (Cmd.Expanded);
      Free (Cmd.Params);

      if Switches /= "" then
         Tmp := Argument_String_To_List (Switches);
         Initialize_Option_Scan (Parser, Tmp, Switch_Char);

         loop
            begin
               S := Getopt (Switches    => "* " & Getopt_Description,
                            Concatenate => False,
                            Parser      => Parser);
               exit when S = ASCII.NUL;

               declare
                  Sw         : constant String :=
                                 Real_Full_Switch (S, Parser);
                  Is_Section : Boolean := False;

               begin
                  if Cmd.Config /= null
                    and then Cmd.Config.Sections /= null
                  then
                     Section_Search :
                     for S in Cmd.Config.Sections'Range loop
                        if Sw = Cmd.Config.Sections (S).all then
                           Section := Cmd.Config.Sections (S);
                           Is_Section := True;

                           exit Section_Search;
                        end if;
                     end loop Section_Search;
                  end if;

                  if not Is_Section then
                     if Section = null then

                        --  Work around some weird cases: some switches may
                        --  expect parameters, but have the same value as
                        --  longer switches: -gnaty3 (-gnaty, parameter=3) and
                        --  -gnatya (-gnatya, no parameter).

                        --  So we are calling add_switch here with parameter
                        --  attached. This will be anyway correctly handled by
                        --  Add_Switch if -gnaty3 is actually provided.

                        if Separator (Parser) = ASCII.NUL then
                           Add_Switch
                             (Cmd, Sw & Parameter (Parser), "");
                        else
                           Add_Switch
                             (Cmd, Sw, Parameter (Parser), Separator (Parser));
                        end if;
                     else
                        if Separator (Parser) = ASCII.NUL then
                           Add_Switch
                             (Cmd, Sw & Parameter (Parser), "",
                              Separator (Parser),
                              Section.all);
                        else
                           Add_Switch
                             (Cmd, Sw,
                              Parameter (Parser),
                              Separator (Parser),
                              Section.all);
                        end if;
                     end if;
                  end if;
               end;

            exception
               when Invalid_Parameter =>

                  --  Add it with no parameter, if that's the way the user
                  --  wants it.

                  --  Specify the separator in all cases, as the switch might
                  --  need to be unaliased, and the alias might contain
                  --  switches with parameters.

                  if Section = null then
                     Add_Switch
                       (Cmd, Switch_Char & Full_Switch (Parser),
                        Separator => Separator (Parser));
                  else
                     Add_Switch
                       (Cmd, Switch_Char & Full_Switch (Parser),
                        Separator => Separator (Parser),
                        Section   => Section.all);
                  end if;
            end;
         end loop;

         Free (Parser);
      end if;
   end Set_Command_Line;

   ----------------
   -- Looking_At --
   ----------------

   function Looking_At
     (Type_Str  : String;
      Index     : Natural;
      Substring : String) return Boolean is
   begin
      return Index + Substring'Length - 1 <= Type_Str'Last
        and then Type_Str (Index .. Index + Substring'Length - 1) = Substring;
   end Looking_At;

   ------------------------
   -- Can_Have_Parameter --
   ------------------------

   function Can_Have_Parameter (S : String) return Boolean is
   begin
      if S'Length <= 1 then
         return False;
      end if;

      case S (S'Last) is
         when '!' | ':' | '?' | '=' =>
            return True;
         when others =>
            return False;
      end case;
   end Can_Have_Parameter;

   -----------------------
   -- Require_Parameter --
   -----------------------

   function Require_Parameter (S : String) return Boolean is
   begin
      if S'Length <= 1 then
         return False;
      end if;

      case S (S'Last) is
         when '!' | ':' | '=' =>
            return True;
         when others =>
            return False;
      end case;
   end Require_Parameter;

   -------------------
   -- Actual_Switch --
   -------------------

   function Actual_Switch (S : String) return String is
   begin
      if S'Length <= 1 then
         return S;
      end if;

      case S (S'Last) is
         when '!' | ':' | '?' | '=' =>
            return S (S'First .. S'Last - 1);
         when others =>
            return S;
      end case;
   end Actual_Switch;

   ----------------------------
   -- For_Each_Simple_Switch --
   ----------------------------

   procedure For_Each_Simple_Switch
     (Cmd       : Command_Line;
      Switch    : String;
      Parameter : String := "";
      Unalias   : Boolean := True)
   is
      function Group_Analysis
        (Prefix : String;
         Group  : String) return Boolean;
      --  Perform the analysis of a group of switches.

      --------------------
      -- Group_Analysis --
      --------------------

      function Group_Analysis
        (Prefix : String;
         Group  : String) return Boolean
      is
         Idx   : Natural;
         Found : Boolean;

      begin
         Idx := Group'First;
         while Idx <= Group'Last loop
            Found := False;

            for S in Cmd.Config.Switches'Range loop
               declare
                  Sw              : constant String :=
                                      Actual_Switch
                                        (Cmd.Config.Switches (S).all);
                  Full            : constant String :=
                                      Prefix & Group (Idx .. Group'Last);
                  Last            : Natural;
                  Param           : Natural;

               begin
                  if Sw'Length >= Prefix'Length

                     --  Verify that sw starts with Prefix

                     and then Looking_At (Sw, Sw'First, Prefix)

                     --  Verify that the group starts with sw

                     and then Looking_At (Full, Full'First, Sw)
                  then
                     Last := Idx + Sw'Length - Prefix'Length - 1;
                     Param := Last + 1;

                     if Can_Have_Parameter (Cmd.Config.Switches (S).all) then

                        --  Include potential parameter to the recursive call.
                        --  Only numbers are allowed.

                        while Last < Group'Last
                          and then Group (Last + 1) in '0' .. '9'
                        loop
                           Last := Last + 1;
                        end loop;
                     end if;

                     if not Require_Parameter (Cmd.Config.Switches (S).all)
                       or else Last >= Param
                     then
                        if Idx = Group'First
                          and then Last = Group'Last
                          and then Last < Param
                        then
                           --  The group only concerns a single switch. Do not
                           --  perform recursive call.

                           --  Note that we still perform a recursive call if
                           --  a parameter is detected in the switch, as this
                           --  is a way to correctly identify such a parameter
                           --  in aliases.

                           return False;
                        end if;

                        Found := True;

                        --  Recursive call, using the detected parameter if any

                        if Last >= Param then
                           For_Each_Simple_Switch
                             (Cmd,
                              Prefix & Group (Idx .. Param - 1),
                              Group (Param .. Last));
                        else
                           For_Each_Simple_Switch
                             (Cmd, Prefix & Group (Idx .. Last), "");
                        end if;

                        Idx := Last + 1;
                        exit;
                     end if;
                  end if;
               end;
            end loop;

            if not Found then
               For_Each_Simple_Switch (Cmd, Prefix & Group (Idx), "");
               Idx := Idx + 1;
            end if;
         end loop;

         return True;
      end Group_Analysis;

   begin
      --  Are we adding a switch that can in fact be expanded through aliases ?
      --  If yes, we add separately each of its expansion.

      --  This takes care of expansions like "-T" -> "-gnatwrs", where the
      --  alias and its expansion do not have the same prefix. Given the order
      --  in which we do things here, the expansion of the alias will itself
      --  be checked for a common prefix and further split into simple switches

      if Unalias
        and then Cmd.Config /= null
        and then Cmd.Config.Aliases /= null
      then
         for A in Cmd.Config.Aliases'Range loop
            if Cmd.Config.Aliases (A).all = Switch
              and then Parameter = ""
            then
               For_Each_Simple_Switch
                 (Cmd, Cmd.Config.Expansions (A).all, "");
               return;
            end if;
         end loop;
      end if;

      --  Are we adding a switch grouping several switches ? If yes, add each
      --  of the simple switches instead.

      if Cmd.Config /= null
        and then Cmd.Config.Prefixes /= null
      then
         for P in Cmd.Config.Prefixes'Range loop
            if Switch'Length > Cmd.Config.Prefixes (P)'Length + 1
              and then Looking_At
                (Switch, Switch'First, Cmd.Config.Prefixes (P).all)
            then
               --  Alias expansion will be done recursively
               if Cmd.Config.Switches = null then
                  for S in Switch'First + Cmd.Config.Prefixes (P)'Length
                            .. Switch'Last
                  loop
                     For_Each_Simple_Switch
                       (Cmd, Cmd.Config.Prefixes (P).all & Switch (S), "");
                  end loop;

                  return;

               elsif Group_Analysis
                 (Cmd.Config.Prefixes (P).all,
                  Switch
                    (Switch'First + Cmd.Config.Prefixes (P)'Length
                      .. Switch'Last))
               then
                  --  Recursive calls already done on each switch of the
                  --  group. Let's return to not call Callback.
                  return;
               end if;
            end if;
         end loop;
      end if;

      --  Test if added switch is a known switch with parameter attached

      if Parameter = ""
        and then Cmd.Config /= null
        and then Cmd.Config.Switches /= null
      then
         for S in Cmd.Config.Switches'Range loop
            declare
               Sw    : constant String :=
                         Actual_Switch (Cmd.Config.Switches (S).all);
               Last  : Natural;
               Param : Natural;

            begin
               --  Verify that switch starts with Sw
               --  What if the "verification" fails???

               if Switch'Length >= Sw'Length
                 and then Looking_At (Switch, Switch'First, Sw)
               then
                  Param := Switch'First + Sw'Length - 1;
                  Last := Param;

                  if Can_Have_Parameter (Cmd.Config.Switches (S).all) then
                     while Last < Switch'Last
                       and then Switch (Last + 1) in '0' .. '9'
                     loop
                        Last := Last + 1;
                     end loop;
                  end if;

                  --  If full Switch is a known switch with attached parameter
                  --  then we use this parameter in the callback.

                  if Last = Switch'Last then
                     Callback
                       (Switch (Switch'First .. Param),
                        Switch (Param + 1 .. Last));
                     return;

                  end if;
               end if;
            end;
         end loop;
      end if;

      Callback (Switch, Parameter);
   end For_Each_Simple_Switch;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ' ';
      Section    : String    := "";
      Add_Before : Boolean   := False)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Add_Switch
        (Cmd, Switch, Parameter, Separator, Section, Add_Before, Success);
   end Add_Switch;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String := "";
      Separator  : Character := ' ';
      Section    : String := "";
      Add_Before : Boolean := False;
      Success    : out Boolean)
   is
      procedure Add_Simple_Switch (Simple : String; Param : String);
      --  Add a new switch that has had all its aliases expanded, and switches
      --  ungrouped. We know there are no more aliases in Switches.

      -----------------------
      -- Add_Simple_Switch --
      -----------------------

      procedure Add_Simple_Switch (Simple : String; Param : String) is
      begin
         if Cmd.Expanded = null then
            Cmd.Expanded := new Argument_List'(1 .. 1 => new String'(Simple));

            if Param /= "" then
               Cmd.Params := new Argument_List'
                 (1 .. 1 => new String'(Separator & Param));

            else
               Cmd.Params := new Argument_List'(1 .. 1 => null);
            end if;

            if Section = "" then
               Cmd.Sections := new Argument_List'(1 .. 1 => null);

            else
               Cmd.Sections := new Argument_List'
                 (1 .. 1 => new String'(Section));
            end if;

         else
            --  Do we already have this switch?

            for C in Cmd.Expanded'Range loop
               if Cmd.Expanded (C).all = Simple
                 and then
                   ((Cmd.Params (C) = null and then Param = "")
                     or else
                       (Cmd.Params (C) /= null
                         and then Cmd.Params (C).all = Separator & Param))
                 and then
                   ((Cmd.Sections (C) = null and then Section = "")
                     or else
                       (Cmd.Sections (C) /= null
                         and then Cmd.Sections (C).all = Section))
               then
                  return;
               end if;
            end loop;

            --  Inserting at least one switch

            Success := True;
            Add (Cmd.Expanded, new String'(Simple), Add_Before);

            if Param /= "" then
               Add
                 (Cmd.Params,
                  new String'(Separator & Param),
                  Add_Before);

            else
               Add
                 (Cmd.Params,
                  null,
                  Add_Before);
            end if;

            if Section = "" then
               Add
                 (Cmd.Sections,
                  null,
                  Add_Before);
            else
               Add
                 (Cmd.Sections,
                  new String'(Section),
                  Add_Before);
            end if;
         end if;
      end Add_Simple_Switch;

      procedure Add_Simple_Switches is
         new For_Each_Simple_Switch (Add_Simple_Switch);

   --  Start of processing for Add_Switch

   begin
      Success := False;
      Add_Simple_Switches (Cmd, Switch, Parameter);
      Free (Cmd.Coalesce);
   end Add_Switch;

   ------------
   -- Remove --
   ------------

   procedure Remove (Line : in out Argument_List_Access; Index : Integer) is
      Tmp : Argument_List_Access := Line;

   begin
      Line := new Argument_List (Tmp'First .. Tmp'Last - 1);

      if Index /= Tmp'First then
         Line (Tmp'First .. Index - 1) := Tmp (Tmp'First .. Index - 1);
      end if;

      Free (Tmp (Index));

      if Index /= Tmp'Last then
         Line (Index .. Tmp'Last - 1) := Tmp (Index + 1 .. Tmp'Last);
      end if;

      Unchecked_Free (Tmp);
   end Remove;

   ---------
   -- Add --
   ---------

   procedure Add
     (Line   : in out Argument_List_Access;
      Str    : String_Access;
      Before : Boolean := False)
   is
      Tmp : Argument_List_Access := Line;

   begin
      if Tmp /= null then
         Line := new Argument_List (Tmp'First .. Tmp'Last + 1);

         if Before then
            Line (Tmp'First)                     := Str;
            Line (Tmp'First + 1 .. Tmp'Last + 1) := Tmp.all;
         else
            Line (Tmp'Range)    := Tmp.all;
            Line (Tmp'Last + 1) := Str;
         end if;

         Unchecked_Free (Tmp);

      else
         Line := new Argument_List'(1 .. 1 => Str);
      end if;
   end Add;

   -------------------
   -- Remove_Switch --
   -------------------

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Remove_All    : Boolean := False;
      Has_Parameter : Boolean := False;
      Section       : String := "")
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Remove_Switch (Cmd, Switch, Remove_All, Has_Parameter, Section, Success);
   end Remove_Switch;

   -------------------
   -- Remove_Switch --
   -------------------

   procedure Remove_Switch
     (Cmd           : in out Command_Line;
      Switch        : String;
      Remove_All    : Boolean := False;
      Has_Parameter : Boolean := False;
      Section       : String  := "";
      Success       : out Boolean)
   is
      procedure Remove_Simple_Switch (Simple : String; Param : String);
      --  Removes a simple switch, with no aliasing or grouping

      --------------------------
      -- Remove_Simple_Switch --
      --------------------------

      procedure Remove_Simple_Switch (Simple : String; Param : String) is
         C : Integer;
         pragma Unreferenced (Param);

      begin
         if Cmd.Expanded /= null then
            C := Cmd.Expanded'First;
            while C <= Cmd.Expanded'Last loop
               if Cmd.Expanded (C).all = Simple
                 and then
                   (Remove_All
                     or else (Cmd.Sections (C) = null
                               and then Section = "")
                     or else (Cmd.Sections (C) /= null
                               and then Section = Cmd.Sections (C).all))
                 and then (not Has_Parameter or else Cmd.Params (C) /= null)
               then
                  Remove (Cmd.Expanded, C);
                  Remove (Cmd.Params, C);
                  Remove (Cmd.Sections, C);
                  Success := True;

                  if not Remove_All then
                     return;
                  end if;

               else
                  C := C + 1;
               end if;
            end loop;
         end if;
      end Remove_Simple_Switch;

      procedure Remove_Simple_Switches is
        new For_Each_Simple_Switch (Remove_Simple_Switch);

   --  Start of processing for Remove_Switch

   begin
      Success := False;
      Remove_Simple_Switches (Cmd, Switch, "", Unalias => not Has_Parameter);
      Free (Cmd.Coalesce);
   end Remove_Switch;

   -------------------
   -- Remove_Switch --
   -------------------

   procedure Remove_Switch
     (Cmd       : in out Command_Line;
      Switch    : String;
      Parameter : String;
      Section   : String  := "")
   is
      procedure Remove_Simple_Switch (Simple : String; Param : String);
      --  Removes a simple switch, with no aliasing or grouping

      --------------------------
      -- Remove_Simple_Switch --
      --------------------------

      procedure Remove_Simple_Switch (Simple : String; Param : String) is
         C : Integer;

      begin
         if Cmd.Expanded /= null then
            C := Cmd.Expanded'First;
            while C <= Cmd.Expanded'Last loop
               if Cmd.Expanded (C).all = Simple
                 and then
                   ((Cmd.Sections (C) = null
                      and then Section = "")
                    or else
                      (Cmd.Sections (C) /= null
                        and then Section = Cmd.Sections (C).all))
                 and then
                   ((Cmd.Params (C) = null and then Param = "")
                      or else
                        (Cmd.Params (C) /= null
                           and then

                           --  Ignore the separator stored in Parameter

                             Cmd.Params (C) (Cmd.Params (C)'First + 1
                                             .. Cmd.Params (C)'Last) =
                           Param))
               then
                  Remove (Cmd.Expanded, C);
                  Remove (Cmd.Params, C);
                  Remove (Cmd.Sections, C);

                  --  The switch is necessarily unique by construction of
                  --  Add_Switch.

                  return;

               else
                  C := C + 1;
               end if;
            end loop;
         end if;
      end Remove_Simple_Switch;

      procedure Remove_Simple_Switches is
         new For_Each_Simple_Switch (Remove_Simple_Switch);

   --  Start of processing for Remove_Switch

   begin
      Remove_Simple_Switches (Cmd, Switch, Parameter);
      Free (Cmd.Coalesce);
   end Remove_Switch;

   --------------------
   -- Group_Switches --
   --------------------

   procedure Group_Switches
     (Cmd      : Command_Line;
      Result   : Argument_List_Access;
      Sections : Argument_List_Access;
      Params   : Argument_List_Access)
   is
      function Compatible_Parameter (Param : String_Access) return Boolean;
      --  True when the parameter can be part of a group

      --------------------------
      -- Compatible_Parameter --
      --------------------------

      function Compatible_Parameter (Param : String_Access) return Boolean is
      begin
         --  No parameter OK

         if Param = null then
            return True;

         --  We need parameters without separators

         elsif Param (Param'First) /= ASCII.NUL then
            return False;

         --  Parameters must be all digits

         else
            for J in Param'First + 1 .. Param'Last loop
               if Param (J) not in '0' .. '9' then
                  return False;
               end if;
            end loop;

            return True;
         end if;
      end Compatible_Parameter;

      --  Local declarations

      Group : Ada.Strings.Unbounded.Unbounded_String;
      First : Natural;
      use type Ada.Strings.Unbounded.Unbounded_String;

   --  Start of processing for Group_Switches

   begin
      if Cmd.Config = null
        or else Cmd.Config.Prefixes = null
      then
         return;
      end if;

      for P in Cmd.Config.Prefixes'Range loop
         Group   := Ada.Strings.Unbounded.Null_Unbounded_String;
         First   := 0;

         for C in Result'Range loop
            if Result (C) /= null
              and then Compatible_Parameter (Params (C))
              and then Looking_At
                (Result (C).all, Result (C)'First, Cmd.Config.Prefixes (P).all)
            then
               --  If we are still in the same section, group the switches

               if First = 0
                 or else
                   (Sections (C) = null
                     and then Sections (First) = null)
                 or else
                   (Sections (C) /= null
                     and then Sections (First) /= null
                     and then Sections (C).all = Sections (First).all)
               then
                  Group :=
                    Group &
                      Result (C)
                        (Result (C)'First + Cmd.Config.Prefixes (P)'Length ..
                         Result (C)'Last);

                  if Params (C) /= null then
                     Group :=
                       Group &
                         Params (C) (Params (C)'First + 1 .. Params (C)'Last);
                     Free (Params (C));
                  end if;

                  if First = 0 then
                     First := C;
                  end if;

                  Free (Result (C));

               else
                  --  We changed section: we put the grouped switches to the
                  --  first place, on continue with the new section.

                  Result (First) :=
                    new String'
                      (Cmd.Config.Prefixes (P).all &
                       Ada.Strings.Unbounded.To_String (Group));
                  Group :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (Result (C)
                       (Result (C)'First + Cmd.Config.Prefixes (P)'Length ..
                            Result (C)'Last));
                  First := C;
               end if;
            end if;
         end loop;

         if First > 0 then
            Result (First) :=
              new String'
                (Cmd.Config.Prefixes (P).all &
                 Ada.Strings.Unbounded.To_String (Group));
         end if;
      end loop;
   end Group_Switches;

   --------------------
   -- Alias_Switches --
   --------------------

   procedure Alias_Switches
     (Cmd    : Command_Line;
      Result : Argument_List_Access;
      Params : Argument_List_Access)
   is
      Found : Boolean;
      First : Natural;

      procedure Check_Cb (Switch : String; Param : String);
      --  Comment required ???

      procedure Remove_Cb (Switch : String; Param : String);
      --  Comment required ???

      --------------
      -- Check_Cb --
      --------------

      procedure Check_Cb (Switch : String; Param : String) is
      begin
         if Found then
            for E in Result'Range loop
               if Result (E) /= null
                 and then
                   (Params (E) = null
                    or else Params (E) (Params (E)'First + 1
                                            .. Params (E)'Last) = Param)
                 and then Result (E).all = Switch
               then
                  return;
               end if;
            end loop;

            Found := False;
         end if;
      end Check_Cb;

      ---------------
      -- Remove_Cb --
      ---------------

      procedure Remove_Cb (Switch : String; Param : String) is
      begin
         for E in Result'Range loop
            if Result (E) /= null
                 and then
                   (Params (E) = null
                    or else Params (E) (Params (E)'First + 1
                                            .. Params (E)'Last) = Param)
              and then Result (E).all = Switch
            then
               if First > E then
                  First := E;
               end if;
               Free (Result (E));
               Free (Params (E));
               return;
            end if;
         end loop;
      end Remove_Cb;

      procedure Check_All is new For_Each_Simple_Switch (Check_Cb);
      procedure Remove_All is new For_Each_Simple_Switch (Remove_Cb);

   --  Start of processing for Alias_Switches

   begin
      if Cmd.Config = null
        or else Cmd.Config.Aliases = null
      then
         return;
      end if;

      for A in Cmd.Config.Aliases'Range loop

         --  Compute the various simple switches that make up the alias. We
         --  split the expansion into as many simple switches as possible, and
         --  then check whether the expanded command line has all of them.

         Found := True;
         Check_All (Cmd, Cmd.Config.Expansions (A).all);

         if Found then
            First := Integer'Last;
            Remove_All (Cmd, Cmd.Config.Expansions (A).all);
            Result (First) := new String'(Cmd.Config.Aliases (A).all);
         end if;
      end loop;
   end Alias_Switches;

   -------------------
   -- Sort_Sections --
   -------------------

   procedure Sort_Sections
     (Line     : GNAT.OS_Lib.Argument_List_Access;
      Sections : GNAT.OS_Lib.Argument_List_Access;
      Params   : GNAT.OS_Lib.Argument_List_Access)
   is
      Sections_List : Argument_List_Access :=
                        new Argument_List'(1 .. 1 => null);
      Found         : Boolean;
      Old_Line      : constant Argument_List := Line.all;
      Old_Sections  : constant Argument_List := Sections.all;
      Old_Params    : constant Argument_List := Params.all;
      Index         : Natural;

   begin
      if Line = null then
         return;
      end if;

      --  First construct a list of all sections

      for E in Line'Range loop
         if Sections (E) /= null then
            Found := False;
            for S in Sections_List'Range loop
               if (Sections_List (S) = null and then Sections (E) = null)
                 or else
                   (Sections_List (S) /= null
                     and then Sections (E) /= null
                     and then Sections_List (S).all = Sections (E).all)
               then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Add (Sections_List, Sections (E));
            end if;
         end if;
      end loop;

      Index := Line'First;

      for S in Sections_List'Range loop
         for E in Old_Line'Range loop
            if (Sections_List (S) = null and then Old_Sections (E) = null)
              or else
                (Sections_List (S) /= null
                  and then Old_Sections (E) /= null
                  and then Sections_List (S).all = Old_Sections (E).all)
            then
               Line (Index) := Old_Line (E);
               Sections (Index) := Old_Sections (E);
               Params (Index) := Old_Params (E);
               Index := Index + 1;
            end if;
         end loop;
      end loop;
   end Sort_Sections;

   -----------
   -- Start --
   -----------

   procedure Start
     (Cmd      : in out Command_Line;
      Iter     : in out Command_Line_Iterator;
      Expanded : Boolean)
   is
   begin
      if Cmd.Expanded = null then
         Iter.List := null;
         return;
      end if;

      --  Reorder the expanded line so that sections are grouped

      Sort_Sections (Cmd.Expanded, Cmd.Sections, Cmd.Params);

      --  Coalesce the switches as much as possible

      if not Expanded
        and then Cmd.Coalesce = null
      then
         Cmd.Coalesce := new Argument_List (Cmd.Expanded'Range);
         for E in Cmd.Expanded'Range loop
            Cmd.Coalesce (E) := new String'(Cmd.Expanded (E).all);
         end loop;

         Cmd.Coalesce_Sections := new Argument_List (Cmd.Sections'Range);
         for E in Cmd.Sections'Range loop
            if Cmd.Sections (E) = null then
               Cmd.Coalesce_Sections (E) := null;
            else
               Cmd.Coalesce_Sections (E) := new String'(Cmd.Sections (E).all);
            end if;
         end loop;

         Cmd.Coalesce_Params := new Argument_List (Cmd.Params'Range);
         for E in Cmd.Params'Range loop
            if Cmd.Params (E) = null then
               Cmd.Coalesce_Params (E) := null;
            else
               Cmd.Coalesce_Params (E) := new String'(Cmd.Params (E).all);
            end if;
         end loop;

         --  Not a clone, since we will not modify the parameters anyway

         Alias_Switches (Cmd, Cmd.Coalesce, Cmd.Coalesce_Params);
         Group_Switches
           (Cmd, Cmd.Coalesce, Cmd.Coalesce_Sections, Cmd.Coalesce_Params);
      end if;

      if Expanded then
         Iter.List     := Cmd.Expanded;
         Iter.Params   := Cmd.Params;
         Iter.Sections := Cmd.Sections;
      else
         Iter.List     := Cmd.Coalesce;
         Iter.Params   := Cmd.Coalesce_Params;
         Iter.Sections := Cmd.Coalesce_Sections;
      end if;

      if Iter.List = null then
         Iter.Current := Integer'Last;
      else
         Iter.Current := Iter.List'First;

         while Iter.Current <= Iter.List'Last
           and then Iter.List (Iter.Current) = null
         loop
            Iter.Current := Iter.Current + 1;
         end loop;
      end if;
   end Start;

   --------------------
   -- Current_Switch --
   --------------------

   function Current_Switch (Iter : Command_Line_Iterator) return String is
   begin
      return Iter.List (Iter.Current).all;
   end Current_Switch;

   --------------------
   -- Is_New_Section --
   --------------------

   function Is_New_Section    (Iter : Command_Line_Iterator) return Boolean is
      Section : constant String := Current_Section (Iter);
   begin
      if Iter.Sections = null then
         return False;
      elsif Iter.Current = Iter.Sections'First
        or else Iter.Sections (Iter.Current - 1) = null
      then
         return Section /= "";
      end if;

      return Section /= Iter.Sections (Iter.Current - 1).all;
   end Is_New_Section;

   ---------------------
   -- Current_Section --
   ---------------------

   function Current_Section (Iter : Command_Line_Iterator) return String is
   begin
      if Iter.Sections = null
        or else Iter.Current > Iter.Sections'Last
        or else Iter.Sections (Iter.Current) = null
      then
         return "";
      end if;

      return Iter.Sections (Iter.Current).all;
   end Current_Section;

   -----------------------
   -- Current_Separator --
   -----------------------

   function Current_Separator (Iter : Command_Line_Iterator) return String is
   begin
      if Iter.Params = null
        or else Iter.Current > Iter.Params'Last
        or else Iter.Params (Iter.Current) = null
      then
         return "";

      else
         declare
            Sep : constant Character :=
              Iter.Params (Iter.Current) (Iter.Params (Iter.Current)'First);
         begin
            if Sep = ASCII.NUL then
               return "";
            else
               return "" & Sep;
            end if;
         end;
      end if;
   end Current_Separator;

   -----------------------
   -- Current_Parameter --
   -----------------------

   function Current_Parameter (Iter : Command_Line_Iterator) return String is
   begin
      if Iter.Params = null
        or else Iter.Current > Iter.Params'Last
        or else Iter.Params (Iter.Current) = null
      then
         return "";

      else
         declare
            P : constant String := Iter.Params (Iter.Current).all;

         begin
            --  Skip separator

            return P (P'First + 1 .. P'Last);
         end;
      end if;
   end Current_Parameter;

   --------------
   -- Has_More --
   --------------

   function Has_More (Iter : Command_Line_Iterator) return Boolean is
   begin
      return Iter.List /= null and then Iter.Current <= Iter.List'Last;
   end Has_More;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Command_Line_Iterator) is
   begin
      Iter.Current := Iter.Current + 1;
      while Iter.Current <= Iter.List'Last
        and then Iter.List (Iter.Current) = null
      loop
         Iter.Current := Iter.Current + 1;
      end loop;
   end Next;

   ----------
   -- Free --
   ----------

   procedure Free (Config : in out Command_Line_Configuration) is
   begin
      if Config /= null then
         Free (Config.Aliases);
         Free (Config.Expansions);
         Free (Config.Prefixes);
         Unchecked_Free (Config);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Cmd : in out Command_Line) is
   begin
      Free (Cmd.Expanded);
      Free (Cmd.Coalesce);
      Free (Cmd.Params);
   end Free;

end GNAT.Command_Line;
