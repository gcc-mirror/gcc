------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Types is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Nat;
   --  Extract two decimal digit value from time stamp

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left = Right) and then String (Left) < String (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Time_Stamp_Type) return Boolean is
      Sleft  : Nat;
      Sright : Nat;

   begin
      if String (Left) = String (Right) then
         return True;

      elsif Left (1) = ' ' or else Right (1) = ' ' then
         return False;
      end if;

      --  In the following code we check for a difference of 2 seconds or less

      --  Recall that the time stamp format is:

      --     Y  Y  Y  Y  M  M  D  D  H  H  M  M  S  S
      --    01 02 03 04 05 06 07 08 09 10 11 12 13 14

      --  Note that we do not bother to worry about shifts in the day.
      --  It seems unlikely that such shifts could ever occur in practice
      --  and even if they do we err on the safe side, ie we say that the time
      --  stamps are different.

      Sright := V (Right, 13) + 60 * (V (Right, 11) + 60 * V (Right, 09));
      Sleft  := V (Left,  13) + 60 * (V (Left,  11) + 60 * V (Left,  09));

      --  So the check is: dates must be the same, times differ 2 sec at most

      return abs (Sleft - Sright) <= 2
         and then String (Left (1 .. 8)) = String (Right (1 .. 8));
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left = Right) and then String (Left) > String (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -------------------
   -- Get_Char_Code --
   -------------------

   function Get_Char_Code (C : Character) return Char_Code is
   begin
      return Char_Code'Val (Character'Pos (C));
   end Get_Char_Code;

   -------------------
   -- Get_Character --
   -------------------

   --  Note: raises Constraint_Error if checks on and C out of range

   function Get_Character (C : Char_Code) return Character is
   begin
      return Character'Val (C);
   end Get_Character;

   --------------------
   -- Get_Hex_String --
   --------------------

   subtype Wordh is Word range 0 .. 15;
   Hex : constant array (Wordh) of Character := "0123456789abcdef";

   function Get_Hex_String (W : Word) return Word_Hex_String is
      X  : Word := W;
      WS : Word_Hex_String;

   begin
      for J in reverse 1 .. 8 loop
         WS (J) := Hex (X mod 16);
         X := X / 16;
      end loop;

      return WS;
   end Get_Hex_String;

   ------------------------
   -- In_Character_Range --
   ------------------------

   function In_Character_Range (C : Char_Code) return Boolean is
   begin
      return (C <= 255);
   end In_Character_Range;

   ---------------------
   -- Make_Time_Stamp --
   ---------------------

   procedure Make_Time_Stamp
     (Year    : Nat;
      Month   : Nat;
      Day     : Nat;
      Hour    : Nat;
      Minutes : Nat;
      Seconds : Nat;
      TS      : out Time_Stamp_Type)
   is
      Z : constant := Character'Pos ('0');

   begin
      TS (01) := Character'Val (Z + Year / 1000);
      TS (02) := Character'Val (Z + (Year / 100) mod 10);
      TS (03) := Character'Val (Z + (Year / 10) mod 10);
      TS (04) := Character'Val (Z + Year mod 10);
      TS (05) := Character'Val (Z + Month / 10);
      TS (06) := Character'Val (Z + Month mod 10);
      TS (07) := Character'Val (Z + Day / 10);
      TS (08) := Character'Val (Z + Day mod 10);
      TS (09) := Character'Val (Z + Hour / 10);
      TS (10) := Character'Val (Z + Hour mod 10);
      TS (11) := Character'Val (Z + Minutes / 10);
      TS (12) := Character'Val (Z + Minutes mod 10);
      TS (13) := Character'Val (Z + Seconds / 10);
      TS (14) := Character'Val (Z + Seconds mod 10);
   end Make_Time_Stamp;

   ----------------------
   -- Split_Time_Stamp --
   ----------------------

   procedure Split_Time_Stamp
     (TS      : Time_Stamp_Type;
      Year    : out Nat;
      Month   : out Nat;
      Day     : out Nat;
      Hour    : out Nat;
      Minutes : out Nat;
      Seconds : out Nat)
   is

   begin
      --     Y  Y  Y  Y  M  M  D  D  H  H  M  M  S  S
      --    01 02 03 04 05 06 07 08 09 10 11 12 13 14

      Year    := 100 * V (TS, 01) + V (TS, 03);
      Month   := V (TS, 05);
      Day     := V (TS, 07);
      Hour    := V (TS, 09);
      Minutes := V (TS, 11);
      Seconds := V (TS, 13);
   end Split_Time_Stamp;

   -------
   -- V --
   -------

   function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Nat is
   begin
      return 10 * (Character'Pos (T (X))     - Character'Pos ('0')) +
                   Character'Pos (T (X + 1)) - Character'Pos ('0');
   end V;

end Types;
