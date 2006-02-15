------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T Y L E S W                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Hostparm; use Hostparm;
with Opt;      use Opt;

package body Stylesw is

   -------------------------------
   -- Reset_Style_Check_Options --
   -------------------------------

   procedure Reset_Style_Check_Options is
   begin
      Style_Check_Indentation         := 0;
      Style_Check_Attribute_Casing    := False;
      Style_Check_Blanks_At_End       := False;
      Style_Check_Blank_Lines         := False;
      Style_Check_Comments            := False;
      Style_Check_DOS_Line_Terminator := False;
      Style_Check_End_Labels          := False;
      Style_Check_Form_Feeds          := False;
      Style_Check_Horizontal_Tabs     := False;
      Style_Check_If_Then_Layout      := False;
      Style_Check_Keyword_Casing      := False;
      Style_Check_Layout              := False;
      Style_Check_Max_Line_Length     := False;
      Style_Check_Max_Nesting_Level   := False;
      Style_Check_Mode_In             := False;
      Style_Check_Order_Subprograms   := False;
      Style_Check_Pragma_Casing       := False;
      Style_Check_References          := False;
      Style_Check_Specs               := False;
      Style_Check_Standard            := False;
      Style_Check_Tokens              := False;
      Style_Check_Xtra_Parens         := False;
   end Reset_Style_Check_Options;

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
      for K in Options'Range loop
         Options (K) := ' ';
      end loop;

      Add (Character'Val (Style_Check_Indentation + Character'Pos ('0')),
           Style_Check_Indentation /= 0);

      Add ('a', Style_Check_Attribute_Casing);
      Add ('b', Style_Check_Blanks_At_End);
      Add ('c', Style_Check_Comments);
      Add ('d', Style_Check_DOS_Line_Terminator);
      Add ('e', Style_Check_End_Labels);
      Add ('f', Style_Check_Form_Feeds);
      Add ('h', Style_Check_Horizontal_Tabs);
      Add ('i', Style_Check_If_Then_Layout);
      Add ('I', Style_Check_Mode_In);
      Add ('k', Style_Check_Keyword_Casing);
      Add ('l', Style_Check_Layout);
      Add ('n', Style_Check_Standard);
      Add ('o', Style_Check_Order_Subprograms);
      Add ('p', Style_Check_Pragma_Casing);
      Add ('r', Style_Check_References);
      Add ('s', Style_Check_Specs);
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
      Set_Style_Check_Options ("3abcefhiklmnprst");
   end Set_Default_Style_Check_Options;

   -----------------------------
   -- Set_Style_Check_Options --
   -----------------------------

   --  Version used when no error checking is required

   procedure Set_Style_Check_Options (Options : String) is
      OK : Boolean;
      EC : Natural;
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

      procedure Add_Img (N : Natural);
      --  Concatenates image of N at end of Style_Msg_Buf

      procedure Bad_Style_Switch (Msg : String);
      --  Called if bad style switch found. Msg is mset in Style_Msg_Buf and
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
         Err_Col := Err_Col + 1;

         case C is
            when '1' .. '9' =>
               Style_Check_Indentation :=
                 Character'Pos (C) - Character'Pos ('0');

            when 'a' =>
               Style_Check_Attribute_Casing    := True;

            when 'b' =>
               Style_Check_Blanks_At_End       := True;

            when 'c' =>
               Style_Check_Comments            := True;

            when 'd' =>
               Style_Check_DOS_Line_Terminator := True;

            when 'e' =>
               Style_Check_End_Labels          := True;

            when 'f' =>
               Style_Check_Form_Feeds          := True;

            when 'h' =>
               Style_Check_Horizontal_Tabs     := True;

            when 'i' =>
               Style_Check_If_Then_Layout      := True;

            when 'I' =>
               Style_Check_Mode_In             := True;

            when 'k' =>
               Style_Check_Keyword_Casing      := True;

            when 'l' =>
               Style_Check_Layout              := True;

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
               Style_Check_Max_Line_Length     := True;
               Style_Max_Line_Length           := 79;

            when 'M' =>
               Style_Max_Line_Length := 0;

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

               Style_Check_Max_Line_Length   := Style_Max_Line_Length /= 0;

            when 'n' =>
               Style_Check_Standard            := True;

            when 'N' =>
               Reset_Style_Check_Options;

            when 'o' =>
               Style_Check_Order_Subprograms   := True;

            when 'p' =>
               Style_Check_Pragma_Casing       := True;

            when 'r' =>
               Style_Check_References          := True;

            when 's' =>
               Style_Check_Specs               := True;

            when 't' =>
               Style_Check_Tokens              := True;

            when 'u' =>
               Style_Check_Blank_Lines         := True;

            when 'x' =>
               Style_Check_Xtra_Parens         := True;

            when ' ' =>
               null;

            when others =>
               Err_Col := Err_Col - 1;
               Style_Msg_Buf (1 .. 21) := "invalid style switch:";
               Style_Msg_Len := 22;
               Style_Msg_Buf (Style_Msg_Len) := C;
               OK := False;
               return;
         end case;
      end loop;

      Style_Check := True;
      OK := True;
   end Set_Style_Check_Options;
end Stylesw;
