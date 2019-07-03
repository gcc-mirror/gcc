------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T Y L E S W                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Hostparm; use Hostparm;
with Opt;      use Opt;
with Output;   use Output;

package body Stylesw is

   --  The following constant defines the default style options for -gnaty

   Default_Style : constant String :=
                     "3" &  -- indentation level is 3
                     "a" &  -- check attribute casing
                     "A" &  -- check array attribute indexes
                     "b" &  -- check no blanks at end of lines
                     "c" &  -- check comment formats
                     "e" &  -- check end/exit labels present
                     "f" &  -- check no form/feeds vertical tabs in source
                     "h" &  -- check no horizontal tabs in source
                     "i" &  -- check if-then layout
                     "k" &  -- check casing rules for keywords
                     "l" &  -- check reference manual layout
                     "m" &  -- check line length <= 79 characters
                     "n" &  -- check casing of package Standard idents
                     "p" &  -- check pragma casing
                     "r" &  -- check casing for identifier references
                     "s" &  -- check separate subprogram specs present
                     "t";   -- check token separation rules

   --  The following constant defines the GNAT style options, showing them
   --  as additions to the standard default style check options.

   GNAT_Style    : constant String := Default_Style &
                     "d" &  -- check no DOS line terminators
                     "I" &  -- check mode IN
                     "S" &  -- check separate lines after THEN or ELSE
                     "u" &  -- check no unnecessary blank lines
                     "x";   -- check extra parentheses around conditionals

   --  Note: we intend GNAT_Style to also include the following, but we do
   --  not yet have the whole tool suite clean with respect to this.

   --                "B" &  -- check boolean operators

   -------------------------------
   -- Reset_Style_Check_Options --
   -------------------------------

   procedure Reset_Style_Check_Options is
   begin
      Style_Check_Indentation           := 0;
      Style_Check_Array_Attribute_Index := False;
      Style_Check_Attribute_Casing      := False;
      Style_Check_Blanks_At_End         := False;
      Style_Check_Blank_Lines           := False;
      Style_Check_Boolean_And_Or        := False;
      Style_Check_Comments              := False;
      Style_Check_DOS_Line_Terminator   := False;
      Style_Check_Mixed_Case_Decls      := False;
      Style_Check_End_Labels            := False;
      Style_Check_Form_Feeds            := False;
      Style_Check_Horizontal_Tabs       := False;
      Style_Check_If_Then_Layout        := False;
      Style_Check_Keyword_Casing        := False;
      Style_Check_Layout                := False;
      Style_Check_Max_Line_Length       := False;
      Style_Check_Max_Nesting_Level     := False;
      Style_Check_Missing_Overriding    := False;
      Style_Check_Mode_In               := False;
      Style_Check_Order_Subprograms     := False;
      Style_Check_Pragma_Casing         := False;
      Style_Check_References            := False;
      Style_Check_Separate_Stmt_Lines   := False;
      Style_Check_Specs                 := False;
      Style_Check_Standard              := False;
      Style_Check_Tokens                := False;
      Style_Check_Xtra_Parens           := False;
   end Reset_Style_Check_Options;

   ---------------------
   -- RM_Column_Check --
   ---------------------

   function RM_Column_Check return Boolean is
   begin
      return Style_Check and Style_Check_Layout;
   end RM_Column_Check;

   ------------------------------
   -- Save_Style_Check_Options --
   ------------------------------

   procedure Save_Style_Check_Options (Options : out Style_Check_Options) is
      P : Natural := 0;

      procedure Add (C : Character; S : Boolean);
      --  Add given character C to string if switch S is true

      procedure Add_Nat (N : Nat);
      --  Add given natural number to string

      ---------
      -- Add --
      ---------

      procedure Add (C : Character; S : Boolean) is
      begin
         if S then
            P := P + 1;
            Options (P) := C;
         end if;
      end Add;

      -------------
      -- Add_Nat --
      -------------

      procedure Add_Nat (N : Nat) is
      begin
         if N > 9 then
            Add_Nat (N / 10);
         end if;

         P := P + 1;
         Options (P) := Character'Val (Character'Pos ('0') + N mod 10);
      end Add_Nat;

   --  Start of processing for Save_Style_Check_Options

   begin
      Add (Character'Val (Style_Check_Indentation + Character'Pos ('0')),
           Style_Check_Indentation /= 0);

      Add ('a', Style_Check_Attribute_Casing);
      Add ('A', Style_Check_Array_Attribute_Index);
      Add ('b', Style_Check_Blanks_At_End);
      Add ('B', Style_Check_Boolean_And_Or);

      if Style_Check_Comments then
         if Style_Check_Comments_Spacing = 2 then
            Add ('c', Style_Check_Comments);
         else
            pragma Assert (Style_Check_Comments_Spacing = 1);
            Add ('C', Style_Check_Comments);
         end if;
      end if;

      Add ('d', Style_Check_DOS_Line_Terminator);
      Add ('D', Style_Check_Mixed_Case_Decls);
      Add ('e', Style_Check_End_Labels);
      Add ('f', Style_Check_Form_Feeds);
      Add ('h', Style_Check_Horizontal_Tabs);
      Add ('i', Style_Check_If_Then_Layout);
      Add ('I', Style_Check_Mode_In);
      Add ('k', Style_Check_Keyword_Casing);
      Add ('l', Style_Check_Layout);
      Add ('n', Style_Check_Standard);
      Add ('o', Style_Check_Order_Subprograms);
      Add ('O', Style_Check_Missing_Overriding);
      Add ('p', Style_Check_Pragma_Casing);
      Add ('r', Style_Check_References);
      Add ('s', Style_Check_Specs);
      Add ('S', Style_Check_Separate_Stmt_Lines);
      Add ('t', Style_Check_Tokens);
      Add ('u', Style_Check_Blank_Lines);
      Add ('x', Style_Check_Xtra_Parens);

      if Style_Check_Max_Line_Length then
         P := P + 1;
         Options (P) := 'M';
         Add_Nat (Style_Max_Line_Length);
      end if;

      if Style_Check_Max_Nesting_Level then
         P := P + 1;
         Options (P) := 'L';
         Add_Nat (Style_Max_Nesting_Level);
      end if;

      pragma Assert (P <= Options'Last);

      while P < Options'Last loop
         P := P + 1;
         Options (P) := ' ';
      end loop;
   end Save_Style_Check_Options;

   -------------------------------------
   -- Set_Default_Style_Check_Options --
   -------------------------------------

   procedure Set_Default_Style_Check_Options is
   begin
      Reset_Style_Check_Options;
      Set_Style_Check_Options (Default_Style);
   end Set_Default_Style_Check_Options;

   ----------------------------------
   -- Set_GNAT_Style_Check_Options --
   ----------------------------------

   procedure Set_GNAT_Style_Check_Options is
   begin
      Reset_Style_Check_Options;
      Set_Style_Check_Options (GNAT_Style);
   end Set_GNAT_Style_Check_Options;

   -----------------------------
   -- Set_Style_Check_Options --
   -----------------------------

   --  Version used when no error checking is required

   procedure Set_Style_Check_Options (Options : String) is
      OK : Boolean;
      EC : Natural;
      pragma Warnings (Off, EC);
   begin
      Set_Style_Check_Options (Options, OK, EC);
      pragma Assert (OK);
   end Set_Style_Check_Options;

   --  Normal version with error checking

   procedure Set_Style_Check_Options
     (Options  : String;
      OK       : out Boolean;
      Err_Col  : out Natural)
   is
      C : Character;

      On : Boolean := True;
      --  Set to False if minus encountered
      --  Set to True if plus encountered

      Last_Option : Character := ' ';
      --  Set to last character encountered

      procedure Add_Img (N : Natural);
      --  Concatenates image of N at end of Style_Msg_Buf

      procedure Bad_Style_Switch (Msg : String);
      --  Called if bad style switch found. Msg is set in Style_Msg_Buf and
      --  Style_Msg_Len. OK is set False.

      -------------
      -- Add_Img --
      -------------

      procedure Add_Img (N : Natural) is
      begin
         if N >= 10 then
            Add_Img (N / 10);
         end if;

         Style_Msg_Len := Style_Msg_Len + 1;
         Style_Msg_Buf (Style_Msg_Len) :=
           Character'Val (N mod 10 + Character'Pos ('0'));
      end Add_Img;

      ----------------------
      -- Bad_Style_Switch --
      ----------------------

      procedure Bad_Style_Switch (Msg : String) is
      begin
         OK := False;
         Style_Msg_Len := Msg'Length;
         Style_Msg_Buf (1 .. Style_Msg_Len) := Msg;
      end Bad_Style_Switch;

   --  Start of processing for Set_Style_Check_Options

   begin
      Err_Col := Options'First;
      while Err_Col <= Options'Last loop
         C := Options (Err_Col);
         Last_Option := C;
         Err_Col := Err_Col + 1;

         --  Turning switches on

         if On then
            case C is
            when '+' =>
               null;

            when '-' =>
               On := False;

            when '0' .. '9' =>
               Style_Check_Indentation :=
                 Character'Pos (C) - Character'Pos ('0');

            when 'a' =>
               Style_Check_Attribute_Casing      := True;

            when 'A' =>
               Style_Check_Array_Attribute_Index := True;

            when 'b' =>
               Style_Check_Blanks_At_End         := True;

            when 'B' =>
               Style_Check_Boolean_And_Or        := True;

            when 'c' =>
               Style_Check_Comments              := True;
               Style_Check_Comments_Spacing      := 2;

            when 'C' =>
               Style_Check_Comments              := True;
               Style_Check_Comments_Spacing      := 1;

            when 'd' =>
               Style_Check_DOS_Line_Terminator   := True;

            when 'D' =>
               Style_Check_Mixed_Case_Decls      := True;

            when 'e' =>
               Style_Check_End_Labels            := True;

            when 'f' =>
               Style_Check_Form_Feeds            := True;

            when 'g' =>
               Set_GNAT_Style_Check_Options;

            when 'h' =>
               Style_Check_Horizontal_Tabs       := True;

            when 'i' =>
               Style_Check_If_Then_Layout        := True;

            when 'I' =>
               Style_Check_Mode_In               := True;

            when 'k' =>
               Style_Check_Keyword_Casing        := True;

            when 'l' =>
               Style_Check_Layout                := True;

            when 'L' =>
               Style_Max_Nesting_Level := 0;

               if Err_Col > Options'Last
                 or else Options (Err_Col) not in '0' .. '9'
               then
                  Bad_Style_Switch ("invalid nesting level");
                  return;
               end if;

               loop
                  Style_Max_Nesting_Level :=
                    Style_Max_Nesting_Level * 10 +
                      Character'Pos (Options (Err_Col)) - Character'Pos ('0');

                  if Style_Max_Nesting_Level > 999 then
                     Bad_Style_Switch
                       ("max nesting level (999) exceeded in style check");
                     return;
                  end if;

                  Err_Col := Err_Col + 1;
                  exit when Err_Col > Options'Last
                    or else Options (Err_Col) not in '0' .. '9';
               end loop;

               Style_Check_Max_Nesting_Level := Style_Max_Nesting_Level /= 0;

            when 'm' =>
               Style_Check_Max_Line_Length       := True;
               Style_Max_Line_Length             := 79;

            when 'M' =>
               Style_Max_Line_Length             := 0;

               if Err_Col > Options'Last
                 or else Options (Err_Col) not in '0' .. '9'
               then
                  Bad_Style_Switch
                    ("invalid line length in style check");
                  return;
               end if;

               loop
                  Style_Max_Line_Length :=
                    Style_Max_Line_Length * 10 +
                      Character'Pos (Options (Err_Col)) - Character'Pos ('0');

                  if Style_Max_Line_Length > Int (Max_Line_Length) then
                     OK := False;
                     Style_Msg_Buf (1 .. 27) := "max line length allowed is ";
                     Style_Msg_Len := 27;
                     Add_Img (Natural (Max_Line_Length));
                     return;
                  end if;

                  Err_Col := Err_Col + 1;
                  exit when Err_Col > Options'Last
                    or else Options (Err_Col) not in '0' .. '9';
               end loop;

               Style_Check_Max_Line_Length       := Style_Max_Line_Length /= 0;

            when 'n' =>
               Style_Check_Standard              := True;

            when 'N' =>
               Reset_Style_Check_Options;

            when 'o' =>
               Style_Check_Order_Subprograms     := True;

            when 'O' =>
               Style_Check_Missing_Overriding    := True;

            when 'p' =>
               Style_Check_Pragma_Casing         := True;

            when 'r' =>
               Style_Check_References            := True;

            when 's' =>
               Style_Check_Specs                 := True;

            when 'S' =>
               Style_Check_Separate_Stmt_Lines   := True;

            when 't' =>
               Style_Check_Tokens                := True;

            when 'u' =>
               Style_Check_Blank_Lines           := True;

            when 'x' =>
               Style_Check_Xtra_Parens           := True;

            when 'y' =>
               Set_Default_Style_Check_Options;

            when ' ' =>
               null;

            when others =>
               if Ignore_Unrecognized_VWY_Switches then
                  Write_Line ("unrecognized switch -gnaty" & C & " ignored");
               else
                  Err_Col := Err_Col - 1;
                  Bad_Style_Switch ("invalid style switch");
                  return;
               end if;
            end case;

         --  Turning switches off

         else
            case C is
            when '+' =>
               On := True;

            when '-' =>
               null;

            when '0' .. '9' =>
               Style_Check_Indentation := 0;

            when 'a' =>
               Style_Check_Attribute_Casing      := False;

            when 'A' =>
               Style_Check_Array_Attribute_Index := False;

            when 'b' =>
               Style_Check_Blanks_At_End         := False;

            when 'B' =>
               Style_Check_Boolean_And_Or        := False;

            when 'c' | 'C' =>
               Style_Check_Comments              := False;

            when 'd' =>
               Style_Check_DOS_Line_Terminator   := False;

            when 'D' =>
               Style_Check_Mixed_Case_Decls      := False;

            when 'e' =>
               Style_Check_End_Labels            := False;

            when 'f' =>
               Style_Check_Form_Feeds            := False;

            when 'g' =>
               Reset_Style_Check_Options;

            when 'h' =>
               Style_Check_Horizontal_Tabs       := False;

            when 'i' =>
               Style_Check_If_Then_Layout        := False;

            when 'I' =>
               Style_Check_Mode_In               := False;

            when 'k' =>
               Style_Check_Keyword_Casing        := False;

            when 'l' =>
               Style_Check_Layout                := False;

            when 'L' =>
               Style_Max_Nesting_Level := 0;

            when 'm' =>
               Style_Check_Max_Line_Length       := False;

            when 'M' =>
               Style_Max_Line_Length             := 0;
               Style_Check_Max_Line_Length       := False;

            when 'n' =>
               Style_Check_Standard              := False;

            when 'o' =>
               Style_Check_Order_Subprograms     := False;

            when 'O' =>
               Style_Check_Missing_Overriding    := False;

            when 'p' =>
               Style_Check_Pragma_Casing         := False;

            when 'r' =>
               Style_Check_References            := False;

            when 's' =>
               Style_Check_Specs                 := False;

            when 'S' =>
               Style_Check_Separate_Stmt_Lines   := False;

            when 't' =>
               Style_Check_Tokens                := False;

            when 'u' =>
               Style_Check_Blank_Lines           := False;

            when 'x' =>
               Style_Check_Xtra_Parens           := False;

            when ' ' =>
               null;

            when others =>
               if Ignore_Unrecognized_VWY_Switches then
                  Write_Line ("unrecognized switch -gnaty-" & C & " ignored");
               else
                  Err_Col := Err_Col - 1;
                  Bad_Style_Switch ("invalid style switch");
                  return;
               end if;
            end case;
         end if;
      end loop;

      --  Turn on style checking if other than N at end of string

      Style_Check := (Last_Option /= 'N');
      OK := True;
   end Set_Style_Check_Options;
end Stylesw;
