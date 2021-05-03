------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . F O R M A T T E D _ S T R I N G                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2021, Free Software Foundation, Inc.         --
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

with Ada.Characters.Handling;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Long_Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with System.Address_Image;

package body GNAT.Formatted_String is

   type F_Kind is (Decimal_Int,                 -- %d %i
                   Unsigned_Decimal_Int,        -- %u
                   Unsigned_Octal,              -- %o
                   Unsigned_Hexadecimal_Int,    -- %x
                   Unsigned_Hexadecimal_Int_Up, -- %X
                   Decimal_Float,               -- %f %F
                   Decimal_Scientific_Float,    -- %e
                   Decimal_Scientific_Float_Up, -- %E
                   Shortest_Decimal_Float,      -- %g
                   Shortest_Decimal_Float_Up,   -- %G
                   Char,                        -- %c
                   Str,                         -- %s
                   Pointer                      -- %p
                  );

   type Sign_Kind is (Neg, Zero, Pos);

   subtype Is_Number is F_Kind range Decimal_Int .. Decimal_Float;

   type F_Sign is (If_Neg, Forced, Space) with Default_Value => If_Neg;

   type F_Base is (None, C_Style, Ada_Style) with Default_Value => None;

   Unset : constant Integer := -1;

   type F_Data is record
      Kind         : F_Kind;
      Width        : Natural := 0;
      Precision    : Integer := Unset;
      Left_Justify : Boolean := False;
      Sign         : F_Sign;
      Base         : F_Base;
      Zero_Pad     : Boolean := False;
      Value_Needed : Natural range 0 .. 2 := 0;
   end record;

   procedure Next_Format
     (Format : Formatted_String;
      F_Spec : out F_Data;
      Start  : out Positive);
   --  Parse the next format specifier, a format specifier has the following
   --  syntax: %[flags][width][.precision][length]specifier

   function Get_Formatted
     (F_Spec : F_Data;
      Value  : String;
      Len    : Positive) return String;
   --  Returns Value formatted given the information in F_Spec

   procedure Raise_Wrong_Format (Format : Formatted_String) with No_Return;
   --  Raise the Format_Error exception which information about the context

   generic
      type Flt is private;

      with procedure Put
        (To   : out String;
         Item : Flt;
         Aft  : Text_IO.Field;
         Exp  : Text_IO.Field);
   function P_Flt_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String;
   --  Generic routine which handles all floating point numbers

   generic
      type Int is private;

      with function To_Integer (Item : Int) return Integer;

      with function Sign (Item : Int) return Sign_Kind;

      with procedure Put
        (To   : out String;
         Item : Int;
         Base : Text_IO.Number_Base);
   function P_Int_Format
     (Format : Formatted_String;
      Var    : Int) return Formatted_String;
   --  Generic routine which handles all the integer numbers

   ---------
   -- "+" --
   ---------

   function "+" (Format : String) return Formatted_String is
   begin
      return Formatted_String'
        (Finalization.Controlled with
           D => new Data'(Format'Length, 1, 1,
             Null_Unbounded_String, 0, 0, (0, 0), Format));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Format : Formatted_String) return String is
      F : String renames Format.D.Format;
      J : Natural renames Format.D.Index;
      R : Unbounded_String := Format.D.Result;

   begin
      --  Make sure we get the remaining character up to the next unhandled
      --  format specifier.

      while (J <= F'Length and then F (J) /= '%')
        or else (J < F'Length - 1 and then F (J + 1) = '%')
      loop
         Append (R, F (J));

         --  If we have two consecutive %, skip the second one

         if F (J) = '%' and then J < F'Length - 1 and then F (J + 1) = '%' then
            J := J + 1;
         end if;

         J := J + 1;
      end loop;

      return To_String (R);
   end "-";

   ---------
   -- "&" --
   ---------

   function "&"
     (Format : Formatted_String;
      Var    : Character) return Formatted_String
   is
      F     : F_Data;
      Start : Positive;

   begin
      Next_Format (Format, F, Start);

      if F.Value_Needed > 0 then
         Raise_Wrong_Format (Format);
      end if;

      case F.Kind is
         when Char =>
            Append (Format.D.Result, Get_Formatted (F, String'(1 => Var), 1));
         when others =>
            Raise_Wrong_Format (Format);
      end case;

      return Format;
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : String) return Formatted_String
   is
      F     : F_Data;
      Start : Positive;

   begin
      Next_Format (Format, F, Start);

      if F.Value_Needed > 0 then
         Raise_Wrong_Format (Format);
      end if;

      case F.Kind is
         when Str =>
            declare
               S : constant String := Get_Formatted (F, Var, Var'Length);
            begin
               if F.Precision = Unset then
                  Append (Format.D.Result, S);
               else
                  Append
                    (Format.D.Result,
                     S (S'First .. S'First + F.Precision - 1));
               end if;
            end;

         when others =>
            Raise_Wrong_Format (Format);
      end case;

      return Format;
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : Boolean) return Formatted_String is
   begin
      return Format & Boolean'Image (Var);
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : Float) return Formatted_String
   is
      function Float_Format is new Flt_Format (Float, Float_Text_IO.Put);
   begin
      return Float_Format (Format, Var);
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : Long_Float) return Formatted_String
   is
      function Float_Format is
        new Flt_Format (Long_Float, Long_Float_Text_IO.Put);
   begin
      return Float_Format (Format, Var);
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : Duration) return Formatted_String
   is
      package Duration_Text_IO is new Text_IO.Fixed_IO (Duration);
      function Duration_Format is
        new P_Flt_Format (Duration, Duration_Text_IO.Put);
   begin
      return Duration_Format (Format, Var);
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : Integer) return Formatted_String
   is
      function Integer_Format is
        new Int_Format (Integer, Integer_Text_IO.Put);
   begin
      return Integer_Format (Format, Var);
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : Long_Integer) return Formatted_String
   is
      function Integer_Format is
        new Int_Format (Long_Integer, Long_Integer_Text_IO.Put);
   begin
      return Integer_Format (Format, Var);
   end "&";

   function "&"
     (Format : Formatted_String;
      Var    : System.Address) return Formatted_String
   is
      A_Img : constant String := System.Address_Image (Var);
      F     : F_Data;
      Start : Positive;

   begin
      Next_Format (Format, F, Start);

      if F.Value_Needed > 0 then
         Raise_Wrong_Format (Format);
      end if;

      case F.Kind is
         when Pointer =>
            Append (Format.D.Result, Get_Formatted (F, A_Img, A_Img'Length));
         when others =>
            Raise_Wrong_Format (Format);
      end case;

      return Format;
   end "&";

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (F : in out Formatted_String) is
   begin
      F.D.Ref_Count := F.D.Ref_Count + 1;
   end Adjust;

   --------------------
   -- Decimal_Format --
   --------------------

   function Decimal_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String
   is
      function Flt_Format is new P_Flt_Format (Flt, Put);
   begin
      return Flt_Format (Format, Var);
   end Decimal_Format;

   -----------------
   -- Enum_Format --
   -----------------

   function Enum_Format
     (Format : Formatted_String;
      Var    : Enum) return Formatted_String is
   begin
      return Format & Enum'Image (Var);
   end Enum_Format;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (F : in out Formatted_String) is
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Data, Data_Access);

      D : Data_Access := F.D;

   begin
      F.D := null;

      D.Ref_Count := D.Ref_Count - 1;

      if D.Ref_Count = 0 then
         Unchecked_Free (D);
      end if;
   end Finalize;

   ------------------
   -- Fixed_Format --
   ------------------

   function Fixed_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String
   is
      function Flt_Format is new P_Flt_Format (Flt, Put);
   begin
      return Flt_Format (Format, Var);
   end Fixed_Format;

   ----------------
   -- Flt_Format --
   ----------------

   function Flt_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String
   is
      function Flt_Format is new P_Flt_Format (Flt, Put);
   begin
      return Flt_Format (Format, Var);
   end Flt_Format;

   -------------------
   -- Get_Formatted --
   -------------------

   function Get_Formatted
     (F_Spec : F_Data;
      Value  : String;
      Len    : Positive) return String
   is
      use Ada.Strings.Fixed;

      Res : Unbounded_String;
      S   : Positive := Value'First;

   begin
      --  Handle the flags

      if F_Spec.Kind in Is_Number then
         if F_Spec.Sign = Forced and then Value (Value'First) /= '-' then
            Append (Res, "+");
         elsif F_Spec.Sign = Space and then Value (Value'First) /= '-' then
            Append (Res, " ");
         end if;

         if Value (Value'First) = '-' then
            Append (Res, "-");
            S := S + 1;
         end if;
      end if;

      --  Zero padding if required and possible

      if F_Spec.Left_Justify = False
        and then F_Spec.Zero_Pad
        and then F_Spec.Width > Len + Value'First - S
      then
         Append (Res, String'((F_Spec.Width - Len + Value'First - S) * '0'));
      end if;

      --  Add the value now

      Append (Res, Value (S .. Value'Last));

      declare
         R : String (1 .. Natural'Max (Natural'Max (F_Spec.Width, Len),
                                       Length (Res))) := (others => ' ');
      begin
         if F_Spec.Left_Justify then
            R (1 .. Length (Res)) := To_String (Res);
         else
            R (R'Last - Length (Res) + 1 .. R'Last) := To_String (Res);
         end if;

         return R;
      end;
   end Get_Formatted;

   ----------------
   -- Int_Format --
   ----------------

   function Int_Format
     (Format : Formatted_String;
      Var    : Int) return Formatted_String
   is
      function Sign (Var : Int) return Sign_Kind is
        (if Var < 0 then Neg elsif Var = 0 then Zero else Pos);

      function To_Integer (Var : Int) return Integer is
        (Integer (Var));

      function Int_Format is new P_Int_Format (Int, To_Integer, Sign, Put);

   begin
      return Int_Format (Format, Var);
   end Int_Format;

   ----------------
   -- Mod_Format --
   ----------------

   function Mod_Format
     (Format : Formatted_String;
      Var    : Int) return Formatted_String
   is
      function Sign (Var : Int) return Sign_Kind is
        (if Var < 0 then Neg elsif Var = 0 then Zero else Pos);

      function To_Integer (Var : Int) return Integer is
        (Integer (Var));

      function Int_Format is new P_Int_Format (Int, To_Integer, Sign, Put);

   begin
      return Int_Format (Format, Var);
   end Mod_Format;

   -----------------
   -- Next_Format --
   -----------------

   procedure Next_Format
     (Format : Formatted_String;
      F_Spec : out F_Data;
      Start  : out Positive)
   is
      F              : String  renames Format.D.Format;
      J              : Natural renames Format.D.Index;
      S              : Natural;
      Width_From_Var : Boolean := False;

   begin
      Format.D.Current := Format.D.Current + 1;
      F_Spec.Value_Needed := 0;

      --  Got to next %

      while (J <= F'Last and then F (J) /= '%')
        or else (J < F'Last - 1 and then F (J + 1) = '%')
      loop
         Append (Format.D.Result, F (J));

         --  If we have two consecutive %, skip the second one

         if F (J) = '%' and then J < F'Last - 1 and then F (J + 1) = '%' then
            J := J + 1;
         end if;

         J := J + 1;
      end loop;

      if F (J) /= '%' or else J = F'Last then
         raise Format_Error with "no format specifier found for parameter"
           & Positive'Image (Format.D.Current);
      end if;

      Start := J;

      J := J + 1;

      --  Check for any flags

      Flags_Check : while J < F'Last loop
         if F (J) = '-' then
            F_Spec.Left_Justify := True;
         elsif F (J) = '+' then
            F_Spec.Sign         := Forced;
         elsif F (J) = ' ' then
            F_Spec.Sign         := Space;
         elsif F (J) = '#' then
            F_Spec.Base         := C_Style;
         elsif F (J) = '~' then
            F_Spec.Base         := Ada_Style;
         elsif F (J) = '0' then
            F_Spec.Zero_Pad     := True;
         else
            exit Flags_Check;
         end if;

         J := J + 1;
      end loop Flags_Check;

      --  Check width if any

      if F (J) in '0' .. '9' then

         --  We have a width parameter

         S := J;

         while J < F'Last and then F (J + 1) in '0' .. '9' loop
            J := J + 1;
         end loop;

         F_Spec.Width := Natural'Value (F (S .. J));

         J := J + 1;

      elsif F (J) = '*' then

         --  The width will be taken from the integer parameter

         F_Spec.Value_Needed := 1;
         Width_From_Var := True;

         J := J + 1;
      end if;

      if F (J) = '.' then

         --  We have a precision parameter

         J := J + 1;

         if F (J) in '0' .. '9' then
            S := J;

            while J < F'Length and then F (J + 1) in '0' .. '9' loop
               J := J + 1;
            end loop;

            if F (J) = '.' then

               --  No precision, 0 is assumed

               F_Spec.Precision := 0;

            else
               F_Spec.Precision := Natural'Value (F (S .. J));
            end if;

            J := J + 1;

         elsif F (J) = '*' then

            --  The prevision will be taken from the integer parameter

            F_Spec.Value_Needed := F_Spec.Value_Needed + 1;
            J := J + 1;
         end if;
      end if;

      --  Skip the length specifier, this is not needed for this implementation
      --  but yet for compatibility reason it is handled.

      Length_Check :
      while J <= F'Last
        and then F (J) in 'h' | 'l' | 'j' | 'z' | 't' | 'L'
      loop
         J := J + 1;
      end loop Length_Check;

      if J > F'Last then
         Raise_Wrong_Format (Format);
      end if;

      --  Read next character which should be the expected type

      case F (J) is
         when 'c'       => F_Spec.Kind := Char;
         when 's'       => F_Spec.Kind := Str;
         when 'd' | 'i' => F_Spec.Kind := Decimal_Int;
         when 'u'       => F_Spec.Kind := Unsigned_Decimal_Int;
         when 'f' | 'F' => F_Spec.Kind := Decimal_Float;
         when 'e'       => F_Spec.Kind := Decimal_Scientific_Float;
         when 'E'       => F_Spec.Kind := Decimal_Scientific_Float_Up;
         when 'g'       => F_Spec.Kind := Shortest_Decimal_Float;
         when 'G'       => F_Spec.Kind := Shortest_Decimal_Float_Up;
         when 'o'       => F_Spec.Kind := Unsigned_Octal;
         when 'x'       => F_Spec.Kind := Unsigned_Hexadecimal_Int;
         when 'X'       => F_Spec.Kind := Unsigned_Hexadecimal_Int_Up;

         when others =>
            raise Format_Error with "unknown format specified for parameter"
              & Positive'Image (Format.D.Current);
      end case;

      J := J + 1;

      if F_Spec.Value_Needed > 0
        and then F_Spec.Value_Needed = Format.D.Stored_Value
      then
         if F_Spec.Value_Needed = 1 then
            if Width_From_Var then
               F_Spec.Width := Format.D.Stack (1);
            else
               F_Spec.Precision := Format.D.Stack (1);
            end if;

         else
            F_Spec.Width := Format.D.Stack (1);
            F_Spec.Precision := Format.D.Stack (2);
         end if;
      end if;
   end Next_Format;

   ------------------
   -- P_Flt_Format --
   ------------------

   function P_Flt_Format
     (Format : Formatted_String;
      Var    : Flt) return Formatted_String
   is
      F      : F_Data;
      Buffer : String (1 .. 50);
      S, E   : Positive := 1;
      Start  : Positive;
      Aft    : Text_IO.Field;

   begin
      Next_Format (Format, F, Start);

      if F.Value_Needed > 0 then
         Raise_Wrong_Format (Format);
      end if;

      if F.Precision = Unset then
         Aft := 6;
      else
         Aft := F.Precision;
      end if;

      case F.Kind is
         when Decimal_Float =>

            Put (Buffer, Var, Aft, Exp => 0);
            S := Strings.Fixed.Index_Non_Blank (Buffer);
            E := Buffer'Last;

         when Decimal_Scientific_Float
            | Decimal_Scientific_Float_Up
         =>
            Put (Buffer, Var, Aft, Exp => 3);
            S := Strings.Fixed.Index_Non_Blank (Buffer);
            E := Buffer'Last;

            if F.Kind = Decimal_Scientific_Float then
               Buffer (S .. E) :=
                 Characters.Handling.To_Lower (Buffer (S .. E));
            end if;

         when Shortest_Decimal_Float
            | Shortest_Decimal_Float_Up
         =>
            --  Without exponent

            Put (Buffer, Var, Aft, Exp => 0);
            S := Strings.Fixed.Index_Non_Blank (Buffer);
            E := Buffer'Last;

            --  Check with exponent

            declare
               Buffer2 : String (1 .. 50);
               S2, E2  : Positive;

            begin
               Put (Buffer2, Var, Aft, Exp => 3);
               S2 := Strings.Fixed.Index_Non_Blank (Buffer2);
               E2 := Buffer2'Last;

               --  If with exponent it is shorter, use it

               if (E2 - S2) < (E - S) then
                  Buffer := Buffer2;
                  S := S2;
                  E := E2;
               end if;
            end;

            if F.Kind = Shortest_Decimal_Float then
               Buffer (S .. E) :=
                 Characters.Handling.To_Lower (Buffer (S .. E));
            end if;

         when others =>
            Raise_Wrong_Format (Format);
      end case;

      Append (Format.D.Result,
        Get_Formatted (F, Buffer (S .. E), Buffer (S .. E)'Length));

      return Format;
   end P_Flt_Format;

   ------------------
   -- P_Int_Format --
   ------------------

   function P_Int_Format
     (Format : Formatted_String;
      Var    : Int) return Formatted_String
   is
      function Handle_Precision return Boolean;
      --  Return True if nothing else to do

      F      : F_Data;
      Buffer : String (1 .. 50);
      S, E   : Positive := 1;
      Len    : Natural := 0;
      Start  : Positive;

      ----------------------
      -- Handle_Precision --
      ----------------------

      function Handle_Precision return Boolean is
      begin
         if F.Precision = 0 and then Sign (Var) = Zero then
            return True;

         elsif F.Precision = Natural'Last then
            null;

         elsif F.Precision > E - S + 1 then
            Len := F.Precision - (E - S + 1);
            Buffer (S - Len .. S - 1) := (others => '0');
            S := S - Len;
         end if;

         return False;
      end Handle_Precision;

   --  Start of processing for P_Int_Format

   begin
      Next_Format (Format, F, Start);

      if Format.D.Stored_Value < F.Value_Needed then
         Format.D.Stored_Value := Format.D.Stored_Value + 1;
         Format.D.Stack (Format.D.Stored_Value) := To_Integer (Var);
         Format.D.Index := Start;
         return Format;
      end if;

      case F.Kind is
         when Unsigned_Octal =>
            if Sign (Var) = Neg then
               Raise_Wrong_Format (Format);
            end if;

            Put (Buffer, Var, Base => 8);
            S := Strings.Fixed.Index (Buffer, "8#") + 2;
            E := Strings.Fixed.Index (Buffer (S .. Buffer'Last), "#") - 1;

            if Handle_Precision then
               return Format;
            end if;

            case F.Base is
               when None      => null;
               when C_Style   => Len := 1;
               when Ada_Style => Len := 3;
            end case;

         when Unsigned_Hexadecimal_Int =>
            if Sign (Var) = Neg then
               Raise_Wrong_Format (Format);
            end if;

            Put (Buffer, Var, Base => 16);
            S := Strings.Fixed.Index (Buffer, "16#") + 3;
            E := Strings.Fixed.Index (Buffer (S .. Buffer'Last), "#") - 1;
            Buffer (S .. E) := Characters.Handling.To_Lower (Buffer (S .. E));

            if Handle_Precision then
               return Format;
            end if;

            case F.Base is
               when None      => null;
               when C_Style   => Len := 2;
               when Ada_Style => Len := 4;
            end case;

         when Unsigned_Hexadecimal_Int_Up =>
            if Sign (Var) = Neg then
               Raise_Wrong_Format (Format);
            end if;

            Put (Buffer, Var, Base => 16);
            S := Strings.Fixed.Index (Buffer, "16#") + 3;
            E := Strings.Fixed.Index (Buffer (S .. Buffer'Last), "#") - 1;

            if Handle_Precision then
               return Format;
            end if;

            case F.Base is
               when None      => null;
               when C_Style   => Len := 2;
               when Ada_Style => Len := 4;
            end case;

         when Unsigned_Decimal_Int =>
            if Sign (Var) = Neg then
               Raise_Wrong_Format (Format);
            end if;

            Put (Buffer, Var, Base => 10);
            S := Strings.Fixed.Index_Non_Blank (Buffer);
            E := Buffer'Last;

            if Handle_Precision then
               return Format;
            end if;

         when Decimal_Int =>
            Put (Buffer, Var, Base => 10);
            S := Strings.Fixed.Index_Non_Blank (Buffer);
            E := Buffer'Last;

            if Handle_Precision then
               return Format;
            end if;

         when Char =>
            S := Buffer'First;
            E := Buffer'First;
            Buffer (S) := Character'Val (To_Integer (Var));

            if Handle_Precision then
               return Format;
            end if;

         when others =>
            Raise_Wrong_Format (Format);
      end case;

      --  Then add base if needed

      declare
         N : String := Get_Formatted (F, Buffer (S .. E), E - S + 1 + Len);
         P : constant Positive :=
               (if F.Left_Justify
                then N'First
                else Natural'Max (Strings.Fixed.Index_Non_Blank (N) - 1,
                                  N'First));
      begin
         case F.Base is
            when None =>
               null;

            when C_Style =>
               case F.Kind is
                  when Unsigned_Octal =>
                     N (P) := 'O';

                  when Unsigned_Hexadecimal_Int =>
                     if F.Left_Justify then
                        N (P .. P + 1) := "Ox";
                     else
                        N (P - 1 .. P) := "0x";
                     end if;

                  when Unsigned_Hexadecimal_Int_Up =>
                     if F.Left_Justify then
                        N (P .. P + 1) := "OX";
                     else
                        N (P - 1 .. P) := "0X";
                     end if;

                  when others =>
                     null;
               end case;

            when Ada_Style =>
               case F.Kind is
                  when Unsigned_Octal =>
                     if F.Left_Justify then
                        N (N'First + 2 .. N'Last) := N (N'First .. N'Last - 2);
                     else
                        N (P .. N'Last - 1) := N (P + 1 .. N'Last);
                     end if;

                     N (N'First .. N'First + 1) := "8#";
                     N (N'Last) := '#';

                  when Unsigned_Hexadecimal_Int
                     | Unsigned_Hexadecimal_Int_Up
                  =>
                     if F.Left_Justify then
                        N (N'First + 3 .. N'Last) := N (N'First .. N'Last - 3);
                     else
                        N (P .. N'Last - 1) := N (P + 1 .. N'Last);
                     end if;

                     N (N'First .. N'First + 2) := "16#";
                     N (N'Last) := '#';

                  when others =>
                     null;
               end case;
         end case;

         Append (Format.D.Result, N);
      end;

      return Format;
   end P_Int_Format;

   ------------------------
   -- Raise_Wrong_Format --
   ------------------------

   procedure Raise_Wrong_Format (Format : Formatted_String) is
   begin
      raise Format_Error with
        "wrong format specified for parameter"
        & Positive'Image (Format.D.Current);
   end Raise_Wrong_Format;

end GNAT.Formatted_String;
