------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . C O M M A N D _ L I N E                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2013, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

package body GNAT.Command_Line is

   --  General note: this entire body could use much more commenting. There
   --  are large sections of uncommented code throughout, and many formal
   --  parameters of local subprograms are not documented at all ???

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
   --
   --  Extra is a character that needs to be added when reporting Full_Switch.
   --  (it will in general be the switch character, for instance '-').
   --  Otherwise, Full_Switch will report 'f' instead of '-f'. In particular,
   --  it needs to be set when reporting an invalid switch or handling '*'.
   --
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
   --  Return the Longest switch from Switches that at least partially matches
   --  Arg. Index_In_Switches is set to 0 if none matches. What are other
   --  parameters??? in particular Param is not always set???

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

   procedure Add
     (Config : in out Command_Line_Configuration;
      Switch : Switch_Definition);
   procedure Add
     (Def   : in out Alias_Definitions_List;
      Alias : Alias_Definition);
   --  Add a new element to Def

   procedure Initialize_Switch_Def
     (Def         : out Switch_Definition;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG");
   --  Initialize [Def] with the contents of the other parameters.
   --  This also checks consistency of the switch parameters, and will raise
   --  Invalid_Switch if they do not match.

   procedure Decompose_Switch
     (Switch         : String;
      Parameter_Type : out Switch_Parameter_Type;
      Switch_Last    : out Integer);
   --  Given a switch definition ("name:" for instance), extracts the type of
   --  parameter that is expected, and the name of the switch

   function Can_Have_Parameter (S : String) return Boolean;
   --  True if S can have a parameter

   function Require_Parameter (S : String) return Boolean;
   --  True if S requires a parameter

   function Actual_Switch (S : String) return String;
   --  Remove any possible trailing '!', ':', '?' and '='

   generic
      with procedure Callback
        (Simple_Switch : String;
         Separator     : String;
         Parameter     : String;
         Index         : Integer);  --  Index in Config.Switches, or -1
   procedure For_Each_Simple_Switch
     (Config    : Command_Line_Configuration;
      Section   : String;
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

   generic
      with function Callback (S : String; Index : Integer) return Boolean;
   procedure Foreach_Switch
     (Config   : Command_Line_Configuration;
      Section  : String);
   --  Iterate over all switches defined in Config, for a specific section.
   --  Index is set to the index in Config.Switches. Stop iterating when
   --  Callback returns False.

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
                            (Character'Pos ('a') - Character'Pos ('A')));
            end if;
         end loop;
      end if;
   end Canonical_Case_File_Name;

   ---------------
   -- Expansion --
   ---------------

   function Expansion (Iterator : Expansion_Iterator) return String is
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

            --  Otherwise continue with the directory at the previous level

            else
               Current := Current - 1;
               It.Current_Depth := Current;
            end if;

         --  If this is a directory, that is neither "." or "..", attempt to
         --  go to the next level.

         elsif Is_Directory
                 (It.Dir_Name (1 .. It.Levels (Current).Name_Last) &
                    S (1 .. Last))
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
         end if;

         --  Check the relative path against the pattern

         --  Note that we try to match also against directory names, since
         --  clients of this function may expect to retrieve directories.

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
      end loop;
   end Expansion;

   ---------------------
   -- Current_Section --
   ---------------------

   function Current_Section
     (Parser : Opt_Parser := Command_Line_Parser) return String
   is
   begin
      if Parser.Current_Section = 1 then
         return "";
      end if;

      for Index in reverse 1 .. Integer'Min (Parser.Current_Argument - 1,
                                             Parser.Section'Last)
      loop
         if Parser.Section (Index) = 0 then
            return Argument (Parser, Index);
         end if;
      end loop;

      return "";
   end Current_Section;

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

   ----------------------
   -- Decompose_Switch --
   ----------------------

   procedure Decompose_Switch
     (Switch         : String;
      Parameter_Type : out Switch_Parameter_Type;
      Switch_Last    : out Integer)
   is
   begin
      if Switch = "" then
         Parameter_Type := Parameter_None;
         Switch_Last := Switch'Last;
         return;
      end if;

      case Switch (Switch'Last) is
         when ':'    =>
            Parameter_Type := Parameter_With_Optional_Space;
            Switch_Last    := Switch'Last - 1;
         when '='    =>
            Parameter_Type := Parameter_With_Space_Or_Equal;
            Switch_Last    := Switch'Last - 1;
         when '!'    =>
            Parameter_Type := Parameter_No_Space;
            Switch_Last    := Switch'Last - 1;
         when '?'    =>
            Parameter_Type := Parameter_Optional;
            Switch_Last    := Switch'Last - 1;
         when others =>
            Parameter_Type := Parameter_None;
            Switch_Last    := Switch'Last;
      end case;
   end Decompose_Switch;

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
      Last   : Natural;
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

         --  Length now marks the separator after the current switch. Last will
         --  mark the last character of the name of the switch.

         if Length = Index + 1 then
            P := Parameter_None;
            Last := Index;
         else
            Decompose_Switch (Switches (Index .. Length - 1), P, Last);
         end if;

         --  If it is the one we searched, it may be a candidate

         if Arg'First + Last - Index <= Arg'Last
           and then Switches (Index .. Last) =
                      Arg (Arg'First .. Arg'First + Last - Index)
           and then Last - Index + 1 > Switch_Length
         then
            Param             := P;
            Index_In_Switches := Index;
            Switch_Length     := Last - Index + 1;
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
      --  element.

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
               --  already been handled.

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

            --  Find the current switch that we did not recognize. This is in
            --  fact difficult because Getopt does not know explicitly about
            --  short and long switches. Ideally, we would want the following
            --  behavior:

            --      * for short switches, with Concatenate:
            --        if -a is not recognized, and the command line has -daf
            --        we should report the invalid switch as "-a".

            --      * for short switches, wihtout Concatenate:
            --        we should report the invalid switch as "-daf".

            --      * for long switches:
            --        if the commadn line is "--long" we should report --long
            --        as unrecongized.

            --  Unfortunately, the fact that long switches start with a
            --  duplicate switch character is just a convention (so we could
            --  have a long switch "-long" for instance). We'll still rely on
            --  this convention here to try and get as helpful an error message
            --  as possible.

            --  Long switch case (starting with double switch character)

            if Arg (Arg'First + 1) = Parser.Switch_Character then
               End_Index := Arg'Last;

            --  Short switch case

            else
               End_Index :=
                 (if Concatenate then Parser.Current_Index else Arg'Last);
            end if;

            if Switches (Switches'First) = '*' then

               --  Always prepend the switch character, so that users know
               --  that this comes from a switch on the command line. This
               --  is especially important when Concatenate is False, since
               --  otherwise the current argument first character is lost.

               if Parser.Section (Parser.Current_Argument) = 0 then

                  --  A section transition should not be returned to the user

                  Dummy := Goto_Next_Argument_In_Section (Parser);
                  goto Restart;

               else
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
            end if;

            if Parser.Current_Index = Arg'First then
               Set_Parameter
                 (Parser.The_Switch,
                  Arg_Num => Parser.Current_Argument,
                  First   => Parser.Current_Index,
                  Last    => End_Index);
            else
               Set_Parameter
                 (Parser.The_Switch,
                  Arg_Num => Parser.Current_Argument,
                  First   => Parser.Current_Index,
                  Last    => End_Index,
                  Extra   => Parser.Switch_Character);
            end if;

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

               --  Case of switch of the form <switch> xxx

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
                     Last    => Arg'Last,
                     Extra   => Parser.Switch_Character);
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

            --  Exit from loop if we have the start of another section

            if Index = Parser.Section'Last
               or else Parser.Section (Index + 1) /= 0
            then
               return;
            end if;
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
         Internal_Initialize_Option_Scan
           (Parser                   => Parser,
            Switch_Char              => Switch_Char,
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
      Parser.Section          := (others => 1);

      --  If we are using sections, we have to preprocess the command line to
      --  delimit them. A section can be repeated, so we just give each item
      --  on the command line a section number

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

               --  A previous section delimiter

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
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation (Opt_Parser_Data, Opt_Parser);
   begin
      if Parser /= null and then Parser /= Command_Line_Parser then
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
      Expanded : String;
      Section  : String := "")
   is
      Def    : Alias_Definition;

   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Def.Alias     := new String'(Switch);
      Def.Expansion := new String'(Expanded);
      Def.Section   := new String'(Section);
      Add (Config.Aliases, Def);
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

   ---------
   -- Add --
   ---------

   procedure Add
     (Config : in out Command_Line_Configuration;
      Switch : Switch_Definition)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Switch_Definitions, Switch_Definitions_List);

      Tmp : Switch_Definitions_List;

   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Tmp := Config.Switches;

      if Tmp = null then
         Config.Switches := new Switch_Definitions (1 .. 1);
      else
         Config.Switches := new Switch_Definitions (1 .. Tmp'Length + 1);
         Config.Switches (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      if Switch.Switch /= null and then Switch.Switch.all = "*" then
         Config.Star_Switch := True;
      end if;

      Config.Switches (Config.Switches'Last) := Switch;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Def   : in out Alias_Definitions_List;
      Alias : Alias_Definition)
   is
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation
          (Alias_Definitions, Alias_Definitions_List);

      Tmp : Alias_Definitions_List := Def;

   begin
      if Tmp = null then
         Def := new Alias_Definitions (1 .. 1);
      else
         Def := new Alias_Definitions (1 .. Tmp'Length + 1);
         Def (1 .. Tmp'Length) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Def (Def'Last) := Alias;
   end Add;

   ---------------------------
   -- Initialize_Switch_Def --
   ---------------------------

   procedure Initialize_Switch_Def
     (Def         : out Switch_Definition;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG")
   is
      P1, P2       : Switch_Parameter_Type := Parameter_None;
      Last1, Last2 : Integer;

   begin
      if Switch /= "" then
         Def.Switch := new String'(Switch);
         Decompose_Switch (Switch, P1, Last1);
      end if;

      if Long_Switch /= "" then
         Def.Long_Switch := new String'(Long_Switch);
         Decompose_Switch (Long_Switch, P2, Last2);
      end if;

      if Switch /= "" and then Long_Switch /= "" then
         if (P1 = Parameter_None and then P2 /= P1)
           or else (P2 = Parameter_None and then P1 /= P2)
           or else (P1 = Parameter_Optional and then P2 /= P1)
           or else (P2 = Parameter_Optional and then P2 /= P1)
         then
            raise Invalid_Switch
              with "Inconsistent parameter types for "
                & Switch & " and " & Long_Switch;
         end if;
      end if;

      if Section /= "" then
         Def.Section := new String'(Section);
      end if;

      if Argument /= "ARG" then
         Def.Argument := new String'(Argument);
      end if;

      if Help /= "" then
         Def.Help := new String'(Help);
      end if;
   end Initialize_Switch_Def;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG")
   is
      Def : Switch_Definition;
   begin
      if Switch /= "" or else Long_Switch /= "" then
         Initialize_Switch_Def
           (Def, Switch, Long_Switch, Help, Section, Argument);
         Add (Config, Def);
      end if;
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Output      : access Boolean;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Value       : Boolean := True)
   is
      Def : Switch_Definition (Switch_Boolean);
   begin
      if Switch /= "" or else Long_Switch /= "" then
         Initialize_Switch_Def (Def, Switch, Long_Switch, Help, Section);
         Def.Boolean_Output := Output.all'Unchecked_Access;
         Def.Boolean_Value  := Value;
         Add (Config, Def);
      end if;
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Output      : access Integer;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Initial     : Integer := 0;
      Default     : Integer := 1;
      Argument    : String := "ARG")
   is
      Def : Switch_Definition (Switch_Integer);
   begin
      if Switch /= "" or else Long_Switch /= "" then
         Initialize_Switch_Def
           (Def, Switch, Long_Switch, Help, Section, Argument);
         Def.Integer_Output  := Output.all'Unchecked_Access;
         Def.Integer_Default := Default;
         Def.Integer_Initial := Initial;
         Add (Config, Def);
      end if;
   end Define_Switch;

   -------------------
   -- Define_Switch --
   -------------------

   procedure Define_Switch
     (Config      : in out Command_Line_Configuration;
      Output      : access GNAT.Strings.String_Access;
      Switch      : String := "";
      Long_Switch : String := "";
      Help        : String := "";
      Section     : String := "";
      Argument    : String := "ARG")
   is
      Def : Switch_Definition (Switch_String);
   begin
      if Switch /= "" or else Long_Switch /= "" then
         Initialize_Switch_Def
           (Def, Switch, Long_Switch, Help, Section, Argument);
         Def.String_Output  := Output.all'Unchecked_Access;
         Add (Config, Def);
      end if;
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

   --------------------
   -- Foreach_Switch --
   --------------------

   procedure Foreach_Switch
     (Config   : Command_Line_Configuration;
      Section  : String)
   is
   begin
      if Config /= null and then Config.Switches /= null then
         for J in Config.Switches'Range loop
            if (Section = "" and then Config.Switches (J).Section = null)
              or else
                (Config.Switches (J).Section /= null
                  and then Config.Switches (J).Section.all = Section)
            then
               exit when Config.Switches (J).Switch /= null
                 and then not Callback (Config.Switches (J).Switch.all, J);

               exit when Config.Switches (J).Long_Switch /= null
                 and then
                   not Callback (Config.Switches (J).Long_Switch.all, J);
            end if;
         end loop;
      end if;
   end Foreach_Switch;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Config      : Command_Line_Configuration;
      Switch_Char : Character := '-';
      Section     : String := "") return String
   is
      Ret : Ada.Strings.Unbounded.Unbounded_String;
      use Ada.Strings.Unbounded;

      function Add_Switch (S : String; Index : Integer) return Boolean;
      --  Add a switch to Ret

      ----------------
      -- Add_Switch --
      ----------------

      function Add_Switch (S : String; Index : Integer) return Boolean is
         pragma Unreferenced (Index);
      begin
         if S = "*" then
            Ret := "*" & Ret;  --  Always first
         elsif S (S'First) = Switch_Char then
            Append (Ret, " " & S (S'First + 1 .. S'Last));
         else
            Append (Ret, " " & S);
         end if;

         return True;
      end Add_Switch;

      Tmp : Boolean;
      pragma Unreferenced (Tmp);

      procedure Foreach is new Foreach_Switch (Add_Switch);

   --  Start of processing for Get_Switches

   begin
      if Config = null then
         return "";
      end if;

      Foreach (Config, Section => Section);

      --  Add relevant aliases

      if Config.Aliases /= null then
         for A in Config.Aliases'Range loop
            if Config.Aliases (A).Section.all = Section then
               Tmp := Add_Switch (Config.Aliases (A).Alias.all, -1);
            end if;
         end loop;
      end if;

      return To_String (Ret);
   end Get_Switches;

   ------------------------
   -- Section_Delimiters --
   ------------------------

   function Section_Delimiters
     (Config : Command_Line_Configuration) return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

   begin
      if Config /= null and then Config.Sections /= null then
         for S in Config.Sections'Range loop
            Append (Result, " " & Config.Sections (S).all);
         end loop;
      end if;

      return To_String (Result);
   end Section_Delimiters;

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
     (Cmd : Command_Line) return Command_Line_Configuration
   is
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
      --  Ensure that the returned switch value contains the Switch_Char prefix
      --  if needed.

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
               if Cmd.Config /= null then

                  --  Do not use Getopt_Description in this case. Otherwise,
                  --  if we have defined a prefix -gnaty, and two switches
                  --  -gnatya and -gnatyL!, we would have a different behavior
                  --  depending on the order of switches:

                  --      -gnatyL1a   =>  -gnatyL with argument "1a"
                  --      -gnatyaL1   =>  -gnatya and -gnatyL with argument "1"

                  --  This is because the call to Getopt below knows nothing
                  --  about prefixes, and in the first case finds a valid
                  --  switch with arguments, so returns it without analyzing
                  --  the argument. In the second case, the switch matches "*",
                  --  and is then decomposed below.

                  --  Note: When a Command_Line object is associated with a
                  --  Command_Line_Config (which is mostly the case for tools
                  --  that let users chose the command line before spawning
                  --  other tools, for instance IDEs), the configuration of
                  --  the switches must be taken from the Command_Line_Config.

                  S := Getopt (Switches    => "* " & Get_Switches (Cmd.Config),
                               Concatenate => False,
                               Parser      => Parser);

               else
                  S := Getopt (Switches    => "* " & Getopt_Description,
                               Concatenate => False,
                               Parser      => Parser);
               end if;

               exit when S = ASCII.NUL;

               declare
                  Sw         : constant String := Real_Full_Switch (S, Parser);
                  Is_Section : Boolean         := False;

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
                        Add_Switch (Cmd, Sw, Parameter (Parser));
                     else
                        Add_Switch
                          (Cmd, Sw, Parameter (Parser),
                           Section => Section.all);
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
                       (Cmd, Switch_Char & Full_Switch (Parser));
                  else
                     Add_Switch
                       (Cmd, Switch_Char & Full_Switch (Parser),
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
      Substring : String) return Boolean
   is
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
     (Config    : Command_Line_Configuration;
      Section   : String;
      Switch    : String;
      Parameter : String := "";
      Unalias   : Boolean := True)
   is
      function Group_Analysis
        (Prefix : String;
         Group  : String) return Boolean;
      --  Perform the analysis of a group of switches

      Found_In_Config : Boolean := False;
      function Is_In_Config
        (Config_Switch : String; Index : Integer) return Boolean;
      --  If Switch is the same as Config_Switch, run the callback and sets
      --  Found_In_Config to True.

      function Starts_With
        (Config_Switch : String; Index : Integer) return Boolean;
      --  if Switch starts with Config_Switch, sets Found_In_Config to True.
      --  The return value is for the Foreach_Switch iterator.

      --------------------
      -- Group_Analysis --
      --------------------

      function Group_Analysis
        (Prefix : String;
         Group  : String) return Boolean
      is
         Idx   : Natural;
         Found : Boolean;

         function Analyze_Simple_Switch
           (Switch : String; Index : Integer) return Boolean;
         --  "Switches" is one of the switch definitions passed to the
         --  configuration, not one of the switches found on the command line.

         ---------------------------
         -- Analyze_Simple_Switch --
         ---------------------------

         function Analyze_Simple_Switch
           (Switch : String; Index : Integer) return Boolean
         is
            pragma Unreferenced (Index);

            Full : constant String := Prefix & Group (Idx .. Group'Last);

            Sw : constant String := Actual_Switch (Switch);
            --  Switches definition minus argument definition

            Last  : Natural;
            Param : Natural;

         begin
            --  Verify that sw starts with Prefix

            if Looking_At (Sw, Sw'First, Prefix)

              --  Verify that the group starts with sw

              and then Looking_At (Full, Full'First, Sw)
            then
               Last  := Idx + Sw'Length - Prefix'Length - 1;
               Param := Last + 1;

               if Can_Have_Parameter (Switch) then

                  --  Include potential parameter to the recursive call. Only
                  --  numbers are allowed.

                  while Last < Group'Last
                    and then Group (Last + 1) in '0' .. '9'
                  loop
                     Last := Last + 1;
                  end loop;
               end if;

               if not Require_Parameter (Switch) or else Last >= Param then
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
                       (Config,
                        Section,
                        Prefix & Group (Idx .. Param - 1),
                        Group (Param .. Last));

                  else
                     For_Each_Simple_Switch
                       (Config, Section, Prefix & Group (Idx .. Last), "");
                  end if;

                  Idx := Last + 1;
                  return False;
               end if;
            end if;

            return True;
         end Analyze_Simple_Switch;

         procedure Foreach is new Foreach_Switch (Analyze_Simple_Switch);

      --  Start of processing for Group_Analysis

      begin
         Idx := Group'First;
         while Idx <= Group'Last loop
            Found := False;
            Foreach (Config, Section);

            if not Found then
               For_Each_Simple_Switch
                 (Config, Section, Prefix & Group (Idx), "");
               Idx := Idx + 1;
            end if;
         end loop;

         return True;
      end Group_Analysis;

      ------------------
      -- Is_In_Config --
      ------------------

      function Is_In_Config
        (Config_Switch : String; Index : Integer) return Boolean
      is
         Last : Natural;
         P    : Switch_Parameter_Type;

      begin
         Decompose_Switch (Config_Switch, P, Last);

         if Config_Switch (Config_Switch'First .. Last) = Switch then
            case P is
               when Parameter_None =>
                  if Parameter = "" then
                     Callback (Switch, "", "", Index => Index);
                     Found_In_Config := True;
                     return False;
                  end if;

               when Parameter_With_Optional_Space =>
                  Callback (Switch, " ", Parameter, Index => Index);
                  Found_In_Config := True;
                  return False;

               when Parameter_With_Space_Or_Equal =>
                  Callback (Switch, "=", Parameter, Index => Index);
                  Found_In_Config := True;
                  return False;

               when Parameter_No_Space =>
                  Callback (Switch, "", Parameter, Index);
                  Found_In_Config := True;
                  return False;

               when Parameter_Optional =>
                  Callback (Switch, "", Parameter, Index);
                  Found_In_Config := True;
                  return False;
            end case;
         end if;

         return True;
      end Is_In_Config;

      -----------------
      -- Starts_With --
      -----------------

      function Starts_With
        (Config_Switch : String; Index : Integer) return Boolean
      is
         Last  : Natural;
         Param : Natural;
         P     : Switch_Parameter_Type;

      begin
         --  This function is called when we believe the parameter was
         --  specified as part of the switch, instead of separately. Thus we
         --  look in the config to find all possible switches.

         Decompose_Switch (Config_Switch, P, Last);

         if Looking_At
              (Switch, Switch'First,
               Config_Switch (Config_Switch'First .. Last))
         then
            --  Set first char of Param, and last char of Switch

            Param := Switch'First + Last;
            Last  := Switch'First + Last - Config_Switch'First;

            case P is

               --  None is already handled in Is_In_Config

               when Parameter_None =>
                  null;

               when Parameter_With_Space_Or_Equal =>
                  if Param <= Switch'Last
                    and then
                      (Switch (Param) = ' ' or else Switch (Param) = '=')
                  then
                     Callback (Switch (Switch'First .. Last),
                               "=", Switch (Param + 1 .. Switch'Last), Index);
                     Found_In_Config := True;
                     return False;
                  end if;

               when Parameter_With_Optional_Space =>
                  if Param <= Switch'Last and then Switch (Param) = ' '  then
                     Param := Param + 1;
                  end if;

                  Callback (Switch (Switch'First .. Last),
                            " ", Switch (Param .. Switch'Last), Index);
                  Found_In_Config := True;
                  return False;

               when Parameter_No_Space | Parameter_Optional =>
                  Callback (Switch (Switch'First .. Last),
                            "", Switch (Param .. Switch'Last), Index);
                  Found_In_Config := True;
                  return False;
            end case;
         end if;
         return True;
      end Starts_With;

      procedure Foreach_In_Config is new Foreach_Switch (Is_In_Config);
      procedure Foreach_Starts_With is new Foreach_Switch (Starts_With);

   --  Start of processing for For_Each_Simple_Switch

   begin
      --  First determine if the switch corresponds to one belonging to the
      --  configuration. If so, run callback and exit.

      --  ??? Is this necessary. On simple tests, we seem to have the same
      --  results with or without this call.

      Foreach_In_Config (Config, Section);

      if Found_In_Config then
         return;
      end if;

      --  If adding a switch that can in fact be expanded through aliases,
      --  add separately each of its expansions.

      --  This takes care of expansions like "-T" -> "-gnatwrs", where the
      --  alias and its expansion do not have the same prefix. Given the order
      --  in which we do things here, the expansion of the alias will itself
      --  be checked for a common prefix and split into simple switches.

      if Unalias
        and then Config /= null
        and then Config.Aliases /= null
      then
         for A in Config.Aliases'Range loop
            if Config.Aliases (A).Section.all = Section
              and then Config.Aliases (A).Alias.all = Switch
              and then Parameter = ""
            then
               For_Each_Simple_Switch
                 (Config, Section, Config.Aliases (A).Expansion.all, "");
               return;
            end if;
         end loop;
      end if;

      --  If adding a switch grouping several switches, add each of the simple
      --  switches instead.

      if Config /= null and then Config.Prefixes /= null then
         for P in Config.Prefixes'Range loop
            if Switch'Length > Config.Prefixes (P)'Length + 1
              and then
                Looking_At (Switch, Switch'First, Config.Prefixes (P).all)
            then
               --  Alias expansion will be done recursively

               if Config.Switches = null then
                  for S in Switch'First + Config.Prefixes (P)'Length
                            .. Switch'Last
                  loop
                     For_Each_Simple_Switch
                       (Config, Section,
                        Config.Prefixes (P).all & Switch (S), "");
                  end loop;

                  return;

               elsif Group_Analysis
                 (Config.Prefixes (P).all,
                  Switch
                    (Switch'First + Config.Prefixes (P)'Length .. Switch'Last))
               then
                  --  Recursive calls already done on each switch of the group:
                  --  Return without executing Callback.

                  return;
               end if;
            end if;
         end loop;
      end if;

      --  Test if added switch is a known switch with parameter attached
      --  instead of being specified separately

      if Parameter = ""
        and then Config /= null
        and then Config.Switches /= null
      then
         Found_In_Config := False;
         Foreach_Starts_With (Config, Section);

         if Found_In_Config then
            return;
         end if;
      end if;

      --  The switch is invalid in the config, but we still want to report it.
      --  The config could, for instance, include "*" to specify it accepts
      --  all switches.

      Callback (Switch, " ", Parameter, Index => -1);
   end For_Each_Simple_Switch;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String    := "";
      Separator  : Character := ASCII.NUL;
      Section    : String    := "";
      Add_Before : Boolean   := False)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Add_Switch (Cmd, Switch, Parameter, Separator,
                  Section, Add_Before, Success);
   end Add_Switch;

   ----------------
   -- Add_Switch --
   ----------------

   procedure Add_Switch
     (Cmd        : in out Command_Line;
      Switch     : String;
      Parameter  : String := "";
      Separator  : Character := ASCII.NUL;
      Section    : String := "";
      Add_Before : Boolean := False;
      Success    : out Boolean)
   is
      procedure Add_Simple_Switch
        (Simple : String;
         Sepa   : String;
         Param  : String;
         Index  : Integer);
      --  Add a new switch that has had all its aliases expanded, and switches
      --  ungrouped. We know there are no more aliases in Switches.

      -----------------------
      -- Add_Simple_Switch --
      -----------------------

      procedure Add_Simple_Switch
        (Simple : String;
         Sepa   : String;
         Param  : String;
         Index  : Integer)
      is
         Sep : Character;

      begin
         if Index = -1
           and then Cmd.Config /= null
           and then not Cmd.Config.Star_Switch
         then
            raise Invalid_Switch
              with "Invalid switch " & Simple;
         end if;

         if Separator /= ASCII.NUL then
            Sep := Separator;

         elsif Sepa = "" then
            Sep := ASCII.NUL;
         else
            Sep := Sepa (Sepa'First);
         end if;

         if Cmd.Expanded = null then
            Cmd.Expanded := new Argument_List'(1 .. 1 => new String'(Simple));

            if Param /= "" then
               Cmd.Params :=
                 new Argument_List'(1 .. 1 => new String'(Sep & Param));
            else
               Cmd.Params := new Argument_List'(1 .. 1 => null);
            end if;

            if Section = "" then
               Cmd.Sections := new Argument_List'(1 .. 1 => null);
            else
               Cmd.Sections :=
                 new Argument_List'(1 .. 1 => new String'(Section));
            end if;

         else
            --  Do we already have this switch?

            for C in Cmd.Expanded'Range loop
               if Cmd.Expanded (C).all = Simple
                 and then
                   ((Cmd.Params (C) = null and then Param = "")
                     or else
                       (Cmd.Params (C) /= null
                         and then Cmd.Params (C).all = Sep & Param))
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
                  new String'(Sep & Param),
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

      --  Local Variables

      Section_Valid : Boolean := False;

   --  Start of processing for Add_Switch

   begin
      if Section /= "" and then Cmd.Config /= null then
         for S in Cmd.Config.Sections'Range loop
            if Section = Cmd.Config.Sections (S).all then
               Section_Valid := True;
               exit;
            end if;
         end loop;

         if not Section_Valid then
            raise Invalid_Section;
         end if;
      end if;

      Success := False;
      Add_Simple_Switches (Cmd.Config, Section, Switch, Parameter);
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
      procedure Remove_Simple_Switch
        (Simple, Separator, Param : String; Index : Integer);
      --  Removes a simple switch, with no aliasing or grouping

      --------------------------
      -- Remove_Simple_Switch --
      --------------------------

      procedure Remove_Simple_Switch
        (Simple, Separator, Param : String; Index : Integer)
      is
         C : Integer;
         pragma Unreferenced (Param, Separator, Index);

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
      Remove_Simple_Switches
        (Cmd.Config, Section, Switch, "", Unalias => not Has_Parameter);
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
      procedure Remove_Simple_Switch
        (Simple, Separator, Param : String; Index : Integer);
      --  Removes a simple switch, with no aliasing or grouping

      --------------------------
      -- Remove_Simple_Switch --
      --------------------------

      procedure Remove_Simple_Switch
        (Simple, Separator, Param : String; Index : Integer)
      is
         pragma Unreferenced (Separator, Index);
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

                          --  Ignore the separator stored in Parameter

                          and then
                             Cmd.Params (C) (Cmd.Params (C)'First + 1
                                             .. Cmd.Params (C)'Last) = Param))
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
      Remove_Simple_Switches (Cmd.Config, Section, Switch, Parameter);
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
      if Cmd.Config = null or else Cmd.Config.Prefixes = null then
         return;
      end if;

      for P in Cmd.Config.Prefixes'Range loop
         Group   := Ada.Strings.Unbounded.Null_Unbounded_String;
         First   := 0;

         for C in Result'Range loop
            if Result (C) /= null
              and then Compatible_Parameter (Params (C))
              and then Looking_At
                         (Result (C).all,
                          Result (C)'First,
                          Cmd.Config.Prefixes (P).all)
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

               --  We changed section: we put the grouped switches to the first
               --  place, on continue with the new section.

               else
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

      procedure Check_Cb (Switch, Separator, Param : String; Index : Integer);
      --  Checks whether the command line contains [Switch]. Sets the global
      --  variable [Found] appropriately. This is called for each simple switch
      --  that make up an alias, to know whether the alias should be applied.

      procedure Remove_Cb (Switch, Separator, Param : String; Index : Integer);
      --  Remove the simple switch [Switch] from the command line, since it is
      --  part of a simpler alias

      --------------
      -- Check_Cb --
      --------------

      procedure Check_Cb
        (Switch, Separator, Param : String; Index : Integer)
      is
         pragma Unreferenced (Separator, Index);

      begin
         if Found then
            for E in Result'Range loop
               if Result (E) /= null
                 and then
                   (Params (E) = null
                     or else Params (E) (Params (E)'First + 1 ..
                                         Params (E)'Last) = Param)
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

      procedure Remove_Cb (Switch, Separator, Param : String; Index : Integer)
      is
         pragma Unreferenced (Separator, Index);

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
      if Cmd.Config = null or else Cmd.Config.Aliases = null then
         return;
      end if;

      for A in Cmd.Config.Aliases'Range loop

         --  Compute the various simple switches that make up the alias. We
         --  split the expansion into as many simple switches as possible, and
         --  then check whether the expanded command line has all of them.

         Found := True;
         Check_All (Cmd.Config,
                    Switch  => Cmd.Config.Aliases (A).Expansion.all,
                    Section => Cmd.Config.Aliases (A).Section.all);

         if Found then
            First := Integer'Last;
            Remove_All (Cmd.Config,
                        Switch  => Cmd.Config.Aliases (A).Expansion.all,
                        Section => Cmd.Config.Aliases (A).Section.all);
            Result (First) := new String'(Cmd.Config.Aliases (A).Alias.all);
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

      Unchecked_Free (Sections_List);
   end Sort_Sections;

   -----------
   -- Start --
   -----------

   procedure Start
     (Cmd      : in out Command_Line;
      Iter     : in out Command_Line_Iterator;
      Expanded : Boolean := False)
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

         Free (Cmd.Coalesce_Sections);
         Cmd.Coalesce_Sections := new Argument_List (Cmd.Sections'Range);
         for E in Cmd.Sections'Range loop
            Cmd.Coalesce_Sections (E) :=
              (if Cmd.Sections (E) = null then null
               else new String'(Cmd.Sections (E).all));
         end loop;

         Free (Cmd.Coalesce_Params);
         Cmd.Coalesce_Params := new Argument_List (Cmd.Params'Range);
         for E in Cmd.Params'Range loop
            Cmd.Coalesce_Params (E) :=
              (if Cmd.Params (E) = null then null
               else new String'(Cmd.Params (E).all));
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
         Iter.Current := Iter.List'First - 1;
         Next (Iter);
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

      else
         return Section /= Iter.Sections (Iter.Current - 1).all;
      end if;
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
         --  Return result, skipping separator

         declare
            P : constant String := Iter.Params (Iter.Current).all;
         begin
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
      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation
          (Switch_Definitions, Switch_Definitions_List);

      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation
          (Alias_Definitions, Alias_Definitions_List);

   begin
      if Config /= null then
         Free (Config.Prefixes);
         Free (Config.Sections);
         Free (Config.Usage);
         Free (Config.Help);
         Free (Config.Help_Msg);

         if Config.Aliases /= null then
            for A in Config.Aliases'Range loop
               Free (Config.Aliases (A).Alias);
               Free (Config.Aliases (A).Expansion);
               Free (Config.Aliases (A).Section);
            end loop;

            Unchecked_Free (Config.Aliases);
         end if;

         if Config.Switches /= null then
            for S in Config.Switches'Range loop
               Free (Config.Switches (S).Switch);
               Free (Config.Switches (S).Long_Switch);
               Free (Config.Switches (S).Help);
               Free (Config.Switches (S).Section);
            end loop;

            Unchecked_Free (Config.Switches);
         end if;

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
      Free (Cmd.Coalesce_Sections);
      Free (Cmd.Coalesce_Params);
      Free (Cmd.Params);
      Free (Cmd.Sections);
   end Free;

   ---------------
   -- Set_Usage --
   ---------------

   procedure Set_Usage
     (Config   : in out Command_Line_Configuration;
      Usage    : String := "[switches] [arguments]";
      Help     : String := "";
      Help_Msg : String := "")
   is
   begin
      if Config = null then
         Config := new Command_Line_Configuration_Record;
      end if;

      Free (Config.Usage);
      Free (Config.Help);
      Free (Config.Help_Msg);

      Config.Usage    := new String'(Usage);
      Config.Help     := new String'(Help);
      Config.Help_Msg := new String'(Help_Msg);
   end Set_Usage;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help (Config : Command_Line_Configuration) is
      function Switch_Name
        (Def     : Switch_Definition;
         Section : String) return String;
      --  Return the "-short, --long=ARG" string for Def.
      --  Returns "" if the switch is not in the section.

      function Param_Name
        (P    : Switch_Parameter_Type;
         Name : String := "ARG") return String;
      --  Return the display for a switch parameter

      procedure Display_Section_Help (Section : String);
      --  Display the help for a specific section ("" is the default section)

      --------------------------
      -- Display_Section_Help --
      --------------------------

      procedure Display_Section_Help (Section : String) is
         Max_Len : Natural := 0;

      begin
         --  ??? Special display for "*"

         New_Line;

         if Section /= "" then
            Put_Line ("Switches after " & Section);
         end if;

         --  Compute size of the switches column

         for S in Config.Switches'Range loop
            Max_Len := Natural'Max
              (Max_Len, Switch_Name (Config.Switches (S), Section)'Length);
         end loop;

         if Config.Aliases /= null then
            for A in Config.Aliases'Range loop
               if Config.Aliases (A).Section.all = Section then
                  Max_Len := Natural'Max
                    (Max_Len, Config.Aliases (A).Alias'Length);
               end if;
            end loop;
         end if;

         --  Display the switches

         for S in Config.Switches'Range loop
            declare
               N : constant String :=
                     Switch_Name (Config.Switches (S), Section);

            begin
               if N /= "" then
                  Put (" ");
                  Put (N);
                  Put ((1 .. Max_Len - N'Length + 1 => ' '));

                  if Config.Switches (S).Help /= null then
                     Put (Config.Switches (S).Help.all);
                  end if;

                  New_Line;
               end if;
            end;
         end loop;

         --  Display the aliases

         if Config.Aliases /= null then
            for A in Config.Aliases'Range loop
               if Config.Aliases (A).Section.all = Section then
                  Put (" ");
                  Put (Config.Aliases (A).Alias.all);
                  Put ((1 .. Max_Len - Config.Aliases (A).Alias'Length + 1
                       => ' '));
                  Put ("Equivalent to " & Config.Aliases (A).Expansion.all);
                  New_Line;
               end if;
            end loop;
         end if;
      end Display_Section_Help;

      ----------------
      -- Param_Name --
      ----------------

      function Param_Name
        (P    : Switch_Parameter_Type;
         Name : String := "ARG") return String
      is
      begin
         case P is
            when Parameter_None =>
               return "";

            when Parameter_With_Optional_Space =>
               return " " & To_Upper (Name);

            when Parameter_With_Space_Or_Equal =>
               return "=" & To_Upper (Name);

            when Parameter_No_Space =>
               return To_Upper (Name);

            when Parameter_Optional =>
               return '[' & To_Upper (Name) & ']';
         end case;
      end Param_Name;

      -----------------
      -- Switch_Name --
      -----------------

      function Switch_Name
        (Def     : Switch_Definition;
         Section : String) return String
      is
         use Ada.Strings.Unbounded;
         Result       : Unbounded_String;
         P1, P2       : Switch_Parameter_Type;
         Last1, Last2 : Integer := 0;

      begin
         if (Section = "" and then Def.Section = null)
           or else (Def.Section /= null and then Def.Section.all = Section)
         then
            if Def.Switch /= null and then Def.Switch.all = "*" then
               return "[any switch]";
            end if;

            if Def.Switch /= null then
               Decompose_Switch (Def.Switch.all, P1, Last1);
               Append (Result, Def.Switch (Def.Switch'First .. Last1));

               if Def.Long_Switch /= null then
                  Decompose_Switch (Def.Long_Switch.all, P2, Last2);
                  Append (Result, ", "
                          & Def.Long_Switch (Def.Long_Switch'First .. Last2));

                  if Def.Argument = null then
                     Append (Result, Param_Name (P2, "ARG"));
                  else
                     Append (Result, Param_Name (P2, Def.Argument.all));
                  end if;

               else
                  if Def.Argument = null then
                     Append (Result, Param_Name (P1, "ARG"));
                  else
                     Append (Result, Param_Name (P1, Def.Argument.all));
                  end if;
               end if;

            --  Def.Switch is null (Long_Switch must be non-null)

            else
               Decompose_Switch (Def.Long_Switch.all, P2, Last2);
               Append (Result,
                       Def.Long_Switch (Def.Long_Switch'First .. Last2));

               if Def.Argument = null then
                  Append (Result, Param_Name (P2, "ARG"));
               else
                  Append (Result, Param_Name (P2, Def.Argument.all));
               end if;
            end if;
         end if;

         return To_String (Result);
      end Switch_Name;

   --  Start of processing for Display_Help

   begin
      if Config = null then
         return;
      end if;

      if Config.Help /= null and then Config.Help.all /= "" then
         Put_Line (Config.Help.all);
      end if;

      if Config.Usage /= null then
         Put_Line ("Usage: "
                   & Base_Name
                     (Ada.Command_Line.Command_Name) & " " & Config.Usage.all);
      else
         Put_Line ("Usage: " & Base_Name (Ada.Command_Line.Command_Name)
                   & " [switches] [arguments]");
      end if;

      if Config.Help_Msg /= null and then Config.Help_Msg.all /= "" then
         Put_Line (Config.Help_Msg.all);

      else
         Display_Section_Help ("");

         if Config.Sections /= null and then Config.Switches /= null then
            for S in Config.Sections'Range loop
               Display_Section_Help (Config.Sections (S).all);
            end loop;
         end if;
      end if;
   end Display_Help;

   ------------
   -- Getopt --
   ------------

   procedure Getopt
     (Config      : Command_Line_Configuration;
      Callback    : Switch_Handler := null;
      Parser      : Opt_Parser := Command_Line_Parser;
      Concatenate : Boolean := True)
   is
      Getopt_Switches : String_Access;
      C               : Character := ASCII.NUL;

      Empty_Name      : aliased constant String := "";
      Current_Section : Integer := -1;
      Section_Name    : not null access constant String := Empty_Name'Access;

      procedure Simple_Callback
        (Simple_Switch : String;
         Separator     : String;
         Parameter     : String;
         Index         : Integer);
      --  Needs comments ???

      procedure Do_Callback (Switch, Parameter : String; Index : Integer);

      -----------------
      -- Do_Callback --
      -----------------

      procedure Do_Callback (Switch, Parameter : String; Index : Integer) is
      begin
         --  Do automatic handling when possible

         if Index /= -1 then
            case Config.Switches (Index).Typ is
               when Switch_Untyped =>
                  null;   --  no automatic handling

               when Switch_Boolean =>
                  Config.Switches (Index).Boolean_Output.all :=
                    Config.Switches (Index).Boolean_Value;
                  return;

               when Switch_Integer =>
                  begin
                     if Parameter = "" then
                        Config.Switches (Index).Integer_Output.all :=
                          Config.Switches (Index).Integer_Default;
                     else
                        Config.Switches (Index).Integer_Output.all :=
                          Integer'Value (Parameter);
                     end if;

                  exception
                     when Constraint_Error =>
                        raise Invalid_Parameter
                          with "Expected integer parameter for '"
                            & Switch & "'";
                  end;

                  return;

               when Switch_String =>
                  Free (Config.Switches (Index).String_Output.all);
                  Config.Switches (Index).String_Output.all :=
                    new String'(Parameter);
                  return;

            end case;
         end if;

         --  Otherwise calls the user callback if one was defined

         if Callback /= null then
            Callback (Switch    => Switch,
                      Parameter => Parameter,
                      Section   => Section_Name.all);
         end if;
      end Do_Callback;

      procedure For_Each_Simple
        is new For_Each_Simple_Switch (Simple_Callback);

      ---------------------
      -- Simple_Callback --
      ---------------------

      procedure Simple_Callback
        (Simple_Switch : String;
         Separator     : String;
         Parameter     : String;
         Index         : Integer)
      is
         pragma Unreferenced (Separator);
      begin
         Do_Callback (Switch    => Simple_Switch,
                      Parameter => Parameter,
                      Index     => Index);
      end Simple_Callback;

   --  Start of processing for Getopt

   begin
      --  Initialize sections

      if Config.Sections = null then
         Config.Sections := new Argument_List'(1 .. 0 => null);
      end if;

      Internal_Initialize_Option_Scan
        (Parser                   => Parser,
         Switch_Char              => Parser.Switch_Character,
         Stop_At_First_Non_Switch => Parser.Stop_At_First,
         Section_Delimiters       => Section_Delimiters (Config));

      Getopt_Switches := new String'
        (Get_Switches (Config, Parser.Switch_Character, Section_Name.all)
         & " h -help");

      --  Initialize output values for automatically handled switches

      for S in Config.Switches'Range loop
         case Config.Switches (S).Typ is
            when Switch_Untyped =>
               null;   --  Nothing to do

            when Switch_Boolean =>
               Config.Switches (S).Boolean_Output.all :=
                 not Config.Switches (S).Boolean_Value;

            when Switch_Integer =>
               Config.Switches (S).Integer_Output.all :=
                 Config.Switches (S).Integer_Initial;

            when Switch_String =>
               if Config.Switches (S).String_Output.all = null then
                  Config.Switches (S).String_Output.all := new String'("");
               end if;
         end case;
      end loop;

      --  For all sections, and all switches within those sections

      loop
         C := Getopt (Switches    => Getopt_Switches.all,
                      Concatenate => Concatenate,
                      Parser      => Parser);

         if C = '*' then
            --  Full_Switch already includes the leading '-'

            Do_Callback (Switch    => Full_Switch (Parser),
                         Parameter => Parameter (Parser),
                         Index     => -1);

         elsif C /= ASCII.NUL then
            if Full_Switch (Parser) = "h"
                 or else
               Full_Switch (Parser) = "-help"
            then
               Display_Help (Config);
               raise Exit_From_Command_Line;
            end if;

            --  Do switch expansion if needed

            For_Each_Simple
              (Config,
               Section   => Section_Name.all,
               Switch    => Parser.Switch_Character & Full_Switch (Parser),
               Parameter => Parameter (Parser));

         else
            if Current_Section = -1 then
               Current_Section := Config.Sections'First;
            else
               Current_Section := Current_Section + 1;
            end if;

            exit when Current_Section > Config.Sections'Last;

            Section_Name := Config.Sections (Current_Section);
            Goto_Section (Section_Name.all, Parser);

            Free (Getopt_Switches);
            Getopt_Switches := new String'
              (Get_Switches
                 (Config, Parser.Switch_Character, Section_Name.all));
         end if;
      end loop;

      Free (Getopt_Switches);

   exception
      when Invalid_Switch =>
         Free (Getopt_Switches);

         --  Message inspired by "ls" on Unix

         Put_Line (Standard_Error,
                   Base_Name (Ada.Command_Line.Command_Name)
                   & ": unrecognized option '"
                   & Full_Switch (Parser)
                   & "'");
         Put_Line (Standard_Error,
                   "Try `"
                   & Base_Name (Ada.Command_Line.Command_Name)
                   & " --help` for more information.");

         raise;

      when others =>
         Free (Getopt_Switches);
         raise;
   end Getopt;

   -----------
   -- Build --
   -----------

   procedure Build
     (Line        : in out Command_Line;
      Args        : out GNAT.OS_Lib.Argument_List_Access;
      Expanded    : Boolean := False;
      Switch_Char : Character := '-')
   is
      Iter  : Command_Line_Iterator;
      Count : Natural := 0;

   begin
      Start (Line, Iter, Expanded => Expanded);
      while Has_More (Iter) loop
         if Is_New_Section (Iter) then
            Count := Count + 1;
         end if;

         Count := Count + 1;
         Next (Iter);
      end loop;

      Args := new Argument_List (1 .. Count);
      Count := Args'First;

      Start (Line, Iter, Expanded => Expanded);
      while Has_More (Iter) loop
         if Is_New_Section (Iter) then
            Args (Count) := new String'(Switch_Char & Current_Section (Iter));
            Count := Count + 1;
         end if;

         Args (Count) := new String'(Current_Switch (Iter)
                                     & Current_Separator (Iter)
                                     & Current_Parameter (Iter));
         Count := Count + 1;
         Next (Iter);
      end loop;
   end Build;

end GNAT.Command_Line;
