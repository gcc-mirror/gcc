------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S C N . N L I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1992-2001 Free Software Foundation, Inc.         --
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

with Uintp;  use Uintp;
with Urealp; use Urealp;

separate (Scn)
procedure Nlit is

   C : Character;
   --  Current source program character

   Base_Char : Character;
   --  Either # or : (character at start of based number)

   Base : Int;
   --  Value of base

   UI_Base : Uint;
   --  Value of base in Uint format

   UI_Int_Value : Uint;
   --  Value of integer scanned by Scan_Integer in Uint format

   UI_Num_Value : Uint;
   --  Value of integer in numeric value being scanned

   Scale : Int;
   --  Scale value for real literal

   UI_Scale : Uint;
   --  Scale in Uint format

   Exponent_Is_Negative : Boolean;
   --  Set true for negative exponent

   Extended_Digit_Value : Int;
   --  Extended digit value

   Point_Scanned : Boolean;
   --  Flag for decimal point scanned in numeric literal

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Digit_Expected;
   --  Signal error of bad digit, Scan_Ptr points to the location at which
   --  the digit was expected on input, and is unchanged on return.

   procedure Scan_Integer;
   --  Procedure to scan integer literal. On entry, Scan_Ptr points to a
   --  digit, on exit Scan_Ptr points past the last character of the integer.
   --  For each digit encountered, UI_Int_Value is multiplied by 10, and the
   --  value of the digit added to the result. In addition, the value in
   --  Scale is decremented by one for each actual digit scanned.

   --------------------------
   -- Error_Digit_Expected --
   --------------------------

   procedure Error_Digit_Expected is
   begin
      Error_Msg_S ("digit expected");
   end Error_Digit_Expected;

   -------------------
   --  Scan_Integer --
   -------------------

   procedure Scan_Integer is
      C : Character;
      --  Next character scanned

   begin
      C := Source (Scan_Ptr);

      --  Loop through digits (allowing underlines)

      loop
         Accumulate_Checksum (C);
         UI_Int_Value :=
           UI_Int_Value * 10 + (Character'Pos (C) - Character'Pos ('0'));
         Scan_Ptr := Scan_Ptr + 1;
         Scale := Scale - 1;
         C := Source (Scan_Ptr);

         if C = '_' then
            Accumulate_Checksum ('_');

            loop
               Scan_Ptr := Scan_Ptr + 1;
               C := Source (Scan_Ptr);
               exit when C /= '_';
               Error_No_Double_Underline;
            end loop;

            if C not in '0' .. '9' then
               Error_Digit_Expected;
               exit;
            end if;

         else
            exit when C not in '0' .. '9';
         end if;
      end loop;

   end Scan_Integer;

----------------------------------
-- Start of Processing for Nlit --
----------------------------------

begin
   Base := 10;
   UI_Base := Uint_10;
   UI_Int_Value := Uint_0;
   Scale := 0;
   Scan_Integer;
   Scale := 0;
   Point_Scanned := False;
   UI_Num_Value := UI_Int_Value;

   --  Various possibilities now for continuing the literal are
   --  period, E/e (for exponent), or :/# (for based literal).

   Scale := 0;
   C := Source (Scan_Ptr);

   if C = '.' then

      --  Scan out point, but do not scan past .. which is a range sequence,
      --  and must not be eaten up scanning a numeric literal.

      while C = '.' and then Source (Scan_Ptr + 1) /= '.' loop
         Accumulate_Checksum ('.');

         if Point_Scanned then
            Error_Msg_S ("duplicate point ignored");
         end if;

         Point_Scanned := True;
         Scan_Ptr := Scan_Ptr + 1;
         C := Source (Scan_Ptr);

         if C not in '0' .. '9' then
            Error_Msg ("real literal cannot end with point", Scan_Ptr - 1);
         else
            Scan_Integer;
            UI_Num_Value := UI_Int_Value;
         end if;
      end loop;

   --  Based literal case. The base is the value we already scanned.
   --  In the case of colon, we insist that the following character
   --  is indeed an extended digit or a period. This catches a number
   --  of common errors, as well as catching the well known tricky
   --  bug otherwise arising from "x : integer range 1 .. 10:= 6;"

   elsif C = '#'
     or else (C = ':' and then
                        (Source (Scan_Ptr + 1) = '.'
                           or else
                         Source (Scan_Ptr + 1) in '0' .. '9'
                           or else
                         Source (Scan_Ptr + 1) in 'A' .. 'Z'
                           or else
                         Source (Scan_Ptr + 1) in 'a' .. 'z'))
   then
      Accumulate_Checksum (C);
      Base_Char := C;
      UI_Base := UI_Int_Value;

      if UI_Base < 2 or else UI_Base > 16 then
         Error_Msg_SC ("base not 2-16");
         UI_Base := Uint_16;
      end if;

      Base := UI_To_Int (UI_Base);
      Scan_Ptr := Scan_Ptr + 1;

      --  Scan out extended integer [. integer]

      C := Source (Scan_Ptr);
      UI_Int_Value := Uint_0;
      Scale := 0;

      loop
         if C in '0' .. '9' then
            Accumulate_Checksum (C);
            Extended_Digit_Value :=
              Int'(Character'Pos (C)) - Int'(Character'Pos ('0'));

         elsif C in 'A' .. 'F' then
            Accumulate_Checksum (Character'Val (Character'Pos (C) + 32));
            Extended_Digit_Value :=
              Int'(Character'Pos (C)) - Int'(Character'Pos ('A')) + 10;

         elsif C in 'a' .. 'f' then
            Accumulate_Checksum (C);
            Extended_Digit_Value :=
              Int'(Character'Pos (C)) - Int'(Character'Pos ('a')) + 10;

         else
            Error_Msg_S ("extended digit expected");
            exit;
         end if;

         if Extended_Digit_Value >= Base then
            Error_Msg_S ("digit >= base");
         end if;

         UI_Int_Value := UI_Int_Value * UI_Base + Extended_Digit_Value;
         Scale := Scale - 1;
         Scan_Ptr := Scan_Ptr + 1;
         C := Source (Scan_Ptr);

         if C = '_' then
            loop
               Accumulate_Checksum ('_');
               Scan_Ptr := Scan_Ptr + 1;
               C := Source (Scan_Ptr);
               exit when C /= '_';
               Error_No_Double_Underline;
            end loop;

         elsif C = '.' then
            Accumulate_Checksum ('.');

            if Point_Scanned then
               Error_Msg_S ("duplicate point ignored");
            end if;

            Scan_Ptr := Scan_Ptr + 1;
            C := Source (Scan_Ptr);
            Point_Scanned := True;
            Scale := 0;

         elsif C = Base_Char then
            Accumulate_Checksum (C);
            Scan_Ptr := Scan_Ptr + 1;
            exit;

         elsif C = '#' or else C = ':' then
            Error_Msg_S ("based number delimiters must match");
            Scan_Ptr := Scan_Ptr + 1;
            exit;

         elsif not Identifier_Char (C) then
            if Base_Char = '#' then
               Error_Msg_S ("missing '#");
            else
               Error_Msg_S ("missing ':");
            end if;

            exit;
         end if;

      end loop;

      UI_Num_Value := UI_Int_Value;
   end if;

   --  Scan out exponent

   if not Point_Scanned then
      Scale := 0;
      UI_Scale := Uint_0;
   else
      UI_Scale := UI_From_Int (Scale);
   end if;

   if Source (Scan_Ptr) = 'e' or else Source (Scan_Ptr) = 'E' then
      Accumulate_Checksum ('e');
      Scan_Ptr := Scan_Ptr + 1;
      Exponent_Is_Negative := False;

      if Source (Scan_Ptr) = '+' then
         Accumulate_Checksum ('+');
         Scan_Ptr := Scan_Ptr + 1;

      elsif Source (Scan_Ptr) = '-' then
         Accumulate_Checksum ('-');

         if not Point_Scanned then
            Error_Msg_S ("negative exponent not allowed for integer literal");
         else
            Exponent_Is_Negative := True;
         end if;

         Scan_Ptr := Scan_Ptr + 1;
      end if;

      UI_Int_Value := Uint_0;

      if Source (Scan_Ptr) in '0' .. '9' then
         Scan_Integer;
      else
         Error_Digit_Expected;
      end if;

      if Exponent_Is_Negative then
         UI_Scale := UI_Scale - UI_Int_Value;
      else
         UI_Scale := UI_Scale + UI_Int_Value;
      end if;
   end if;

   --  Case of real literal to be returned

   if Point_Scanned then
      Token := Tok_Real_Literal;
      Token_Node := New_Node (N_Real_Literal, Token_Ptr);
      Set_Realval (Token_Node,
        UR_From_Components (
          Num   => UI_Num_Value,
          Den   => -UI_Scale,
          Rbase => Base));

   --  Case of integer literal to be returned

   else
      Token := Tok_Integer_Literal;
      Token_Node := New_Node (N_Integer_Literal, Token_Ptr);

      if UI_Scale = 0 then
         Set_Intval (Token_Node, UI_Num_Value);

      --  Avoid doing possibly expensive calculations in cases like
      --  parsing 163E800_000# when semantics will not be done anyway.
      --  This is especially useful when parsing garbled input.

      elsif Operating_Mode /= Check_Syntax
        and then (Serious_Errors_Detected = 0 or else Try_Semantics)
      then
         Set_Intval (Token_Node, UI_Num_Value * UI_Base ** UI_Scale);

      else
         Set_Intval (Token_Node, No_Uint);
      end if;

   end if;

   return;

end Nlit;
