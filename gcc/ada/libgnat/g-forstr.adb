------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . F O R M A T T E D _ S T R I N G                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2014-2025, Free Software Foundation, Inc.         --
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
with Ada.Strings;
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
                   G_Specifier,                 -- %g
                   G_Specifier_Up,              -- %G
                   Char,                        -- %c
                   Str,                         -- %s
                   Pointer                      -- %p
                  );

   type Sign_Kind is (Neg, Zero, Pos);

   subtype Is_Number is F_Kind range Decimal_Int .. G_Specifier_Up;

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

   type Notation is (Decimal, Scientific);

   procedure Advance_And_Accumulate_Until_Next_Specifier
     (Format : Formatted_String);
   --  Advance Format.D.Index until either the next format specifier is
   --  encountered, or the end of Format.D.Format is reached. The characters
   --  advanced over are appended to Format.D.Result.

   procedure Next_Format
     (Format : Formatted_String;
      F_Spec : out F_Data;
      Start  : out Positive);
   --  Parse the next format specifier, a format specifier has the following
   --  syntax: %[flags][width][.precision][length]specifier

   procedure Determine_Notation_And_Aft
     (Exponent                : Integer;
      Precision               : Text_IO.Field;
      Nota                    : out Notation;
      Aft                     : out Text_IO.Field);
   --  Determine whether to use scientific or decimal notation and the value of
   --  Aft given the exponent and precision of a real number, as described in
   --  the C language specification, section 7.21.6.1.

   function Get_Formatted
     (F_Spec : F_Data;
      Value  : String;
      Len    : Positive) return String;
   --  Returns Value formatted given the information in F_Spec

   procedure Increment_Integral_Part
     (Buffer              : in out String;
      First_Non_Blank     : in out Positive;
      Last_Digit_Position : Positive);
   --  Buffer must contain the textual representation of a number.
   --  Last_Digit_Position must be the position of the rightmost digit of the
   --  integral part. Buffer must have at least one padding blank. Increment
   --  the integral part.

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

   procedure Remove_Extraneous_Decimal_Digit
     (Textual_Rep     : in out String;
      First_Non_Blank : in out Positive);
   --  Remove the unique digit to the right of the point in Textual_Rep

   procedure Trim_Fractional_Part
     (Textual_Rep     : in out String;
      First_Non_Blank : in out Positive);
   --  Remove trailing zeros from Textual_Rep, which must be the textual
   --  representation of a real number. If the fractional part only contains
   --  zeros, also remove the point.

   ---------
   -- "+" --
   ---------

   function "+" (Format : String) return Formatted_String is
   begin
      return Formatted_String'
        (Finalization.Controlled with
           D => new Data'(Format'Length, 1, 1,
             Null_Unbounded_String, 0, 0, [0, 0], Format));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Format : Formatted_String) return String is
   begin
      --  Make sure we get the remaining character up to the next unhandled
      --  format specifier.

      Advance_And_Accumulate_Until_Next_Specifier (Format);

      return To_String (Format.D.Result);
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

   -------------------------------------------------
   -- Advance_And_Accumulate_Until_Next_Specifier --
   -------------------------------------------------

   procedure Advance_And_Accumulate_Until_Next_Specifier
     (Format : Formatted_String)
   is
   begin
      loop
         if Format.D.Index > Format.D.Format'Last then
            exit;
         end if;

         if Format.D.Format (Format.D.Index) /= '%' then
            Append (Format.D.Result, Format.D.Format (Format.D.Index));
            Format.D.Index := Format.D.Index + 1;
         elsif Format.D.Index + 1 <= Format.D.Format'Last
           and then Format.D.Format (Format.D.Index + 1) = '%'
         then
            Append (Format.D.Result, '%');
            Format.D.Index := Format.D.Index + 2;
         else
            exit;
         end if;
      end loop;
   end Advance_And_Accumulate_Until_Next_Specifier;

   --------------------------------
   -- Determine_Notation_And_Aft --
   --------------------------------

   procedure Determine_Notation_And_Aft
     (Exponent  : Integer;
      Precision : Text_IO.Field;
      Nota      : out Notation;
      Aft       : out Text_IO.Field)
   is
      --  The constants use the same names as those from the C specification
      --  in order to match the description of the predicate.
      P : constant Text_IO.Field := (if Precision /= 0 then Precision else 1);
      X : constant Integer       := Exponent;
   begin
      if P > X and X >= -4 then
         Nota := Decimal;
         Aft  := P - (X + 1);
      else
         Nota := Scientific;
         Aft  := P - 1;
      end if;
   end Determine_Notation_And_Aft;

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

      if not F_Spec.Left_Justify
        and then F_Spec.Zero_Pad
        and then F_Spec.Width > Len + Value'First - S
      then
         Append (Res, String'((F_Spec.Width - (Len + Value'First - S)) * '0'));
      end if;

      --  Add the value now

      Append (Res, Value (S .. Value'Last));

      declare
         R : String (1 .. Natural'Max (Natural'Max (F_Spec.Width, Len),
                                       Length (Res))) := [others => ' '];
      begin
         if F_Spec.Left_Justify then
            R (1 .. Length (Res)) := To_String (Res);
         else
            R (R'Last - Length (Res) + 1 .. R'Last) := To_String (Res);
         end if;

         return R;
      end;
   end Get_Formatted;

   -----------------------------
   -- Increment_Integral_Part --
   -----------------------------

   procedure Increment_Integral_Part
     (Buffer              : in out String;
      First_Non_Blank     : in out Positive;
      Last_Digit_Position : Positive)
   is
      Cursor : Natural := Last_Digit_Position;
   begin
      while Buffer (Cursor) = '9' loop
         Buffer (Cursor) := '0';
         Cursor          := Cursor - 1;
      end loop;

      pragma Assert (Cursor > 0);

      if Buffer (Cursor) in '0' .. '8' then
         Buffer (Cursor) := Character'Succ (Buffer (Cursor));
      else
         Ada.Strings.Fixed.Insert
           (Buffer,
            Cursor + 1,
            "1");
         First_Non_Blank := First_Non_Blank - 1;
      end if;
   end Increment_Integral_Part;

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

      Advance_And_Accumulate_Until_Next_Specifier (Format);

      if J >= F'Last or else F (J) /= '%'  then
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
         when 'g'       => F_Spec.Kind := G_Specifier;
         when 'G'       => F_Spec.Kind := G_Specifier_Up;
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
      procedure Compute_Exponent
        (Var      : Flt;
         Valid    : out Boolean;
         Exponent : out Integer);
      --  If Var is invalid (for example, a NaN of an inf), set Valid False and
      --  set Exponent to 0. Otherwise, set Valid True, and store the exponent
      --  of the scientific notation representation of Var in Exponent. The
      --  exponent can also be defined as:
      --  - If Var = 0, 0.
      --  - Otherwise, Floor (Log_10 (Abs (Var))).

      procedure Format_With_Notation
        (Var : Flt;
         Nota : Notation;
         Aft : Text_IO.Field;
         Buffer : out String);
      --  Fill buffer with the formatted value of Var following the notation
      --  specified through Nota.

      procedure Handle_G_Specifier
        (Buffer          : out String;
         First_Non_Blank : out Positive;
         Aft             : Text_IO.Field);
      --  Fill Buffer with the formatted value of Var according to the rules of
      --  the "%g" specifier. Buffer is right-justified and padded with blanks.

      ----------------------
      -- Compute_Exponent --
      ----------------------

      procedure Compute_Exponent
        (Var      : Flt;
         Valid    : out Boolean;
         Exponent : out Integer)
      is
         --  The way the exponent is computed is convoluted. It is not possible
         --  to use the logarithm in base 10 of Var and floor it, because the
         --  math functions for this are not available for fixed point types.
         --  Instead, use the generic Put procedure to produce a scientific
         --  representation of Var, and parse the exponent part of that back
         --  into an Integer.
         Scientific_Rep : String (1 .. 50);

         E_Position : Natural;
      begin
         Put (Scientific_Rep, Var, Aft => 1, Exp => 1);

         E_Position := Ada.Strings.Fixed.Index (Scientific_Rep, "E");

         if E_Position = 0 then
            Valid    := False;
            Exponent := 0;
         else
            Valid    := True;
            Exponent :=
              Integer'Value
                (Scientific_Rep (E_Position + 1 .. Scientific_Rep'Last));
         end if;
      end Compute_Exponent;

      --------------------------
      -- Format_With_Notation --
      --------------------------

      procedure Format_With_Notation
        (Var : Flt;
         Nota : Notation;
         Aft : Text_IO.Field;
         Buffer : out String)
      is
         Exp : constant Text_IO.Field :=
           (case Nota is when Decimal => 0, when Scientific => 3);
      begin
         Put (Buffer, Var, Aft, Exp);
      end Format_With_Notation;

      ------------------------
      -- Handle_G_Specifier --
      ------------------------

      procedure Handle_G_Specifier
        (Buffer          : out String;
         First_Non_Blank : out Positive;
         Aft             : Text_IO.Field)
      is
         --  There is nothing that is directly equivalent to the "%g" specifier
         --  in the standard Ada functionality provided by Ada.Text_IO. The
         --  procedure Put will still be used, but significant postprocessing
         --  will be performed on the output of that procedure.

         --  The following code is intended to match the behavior of C's printf
         --  for %g, as described by paragraph "7.21.6.1 The fprintf function"
         --  of the C language specification.

         --  As explained in the C specification, we're going to have to make a
         --  choice between decimal notation and scientific notation. One of
         --  the elements we need in order to make that choice is the value of
         --  the exponent in the decimal representation of Var. We will store
         --  that value in Exponent.
         Exponent : Integer;
         Valid    : Boolean;

         Nota : Notation;

         --  The value of the formal Aft comes from the precision specifier in
         --  the format string. For %g, the precision specifier corresponds to
         --  the number of significant figures desired, whereas the formal Aft
         --  in Put corresponds to the number of digits after the point.
         --  Effective_Aft is what will be passed to Put as Aft in order to
         --  respect the semantics of %g.
         Effective_Aft : Text_IO.Field;

         Textual_Rep : String (Buffer'Range);
      begin
         Compute_Exponent (Var, Valid, Exponent);

         Determine_Notation_And_Aft
           (Exponent, Aft, Nota, Effective_Aft);

         Format_With_Notation (Var, Nota, Effective_Aft, Textual_Rep);

         First_Non_Blank := Strings.Fixed.Index_Non_Blank (Textual_Rep);

         if not Valid then
            null;
         elsif Effective_Aft = 0 then
            --  Special case: it is possible at this point that Effective_Aft
            --  is zero. But when Put is passed zero through Aft, it still
            --  outputs one digit after the point. See the reference manual,
            --  A.10.9.25.

            Remove_Extraneous_Decimal_Digit (Textual_Rep, First_Non_Blank);
         else
            Trim_Fractional_Part
              (Textual_Rep, First_Non_Blank);
         end if;

         Buffer := Textual_Rep;
      end Handle_G_Specifier;

      --  Local variables

      F      : F_Data;
      Buffer : String (1 .. 50);
      S, E   : Positive := 1;
      Start  : Positive;
      Aft    : Text_IO.Field;

   --  Start of processing for P_Flt_Format

   begin
      Next_Format (Format, F, Start);

      if F.Value_Needed /= Format.D.Stored_Value then
         Raise_Wrong_Format (Format);
      end if;
      Format.D.Stored_Value := 0;

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

         when G_Specifier
            | G_Specifier_Up
         =>
            Handle_G_Specifier (Buffer, S, Aft);
            E := Buffer'Last;

            if F.Kind = G_Specifier then
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
            Buffer (S - Len .. S - 1) := [others => '0'];
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
      Format.D.Stored_Value := 0;

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

   -------------------------------------
   -- Remove_Extraneous_Decimal_Digit --
   -------------------------------------

   procedure Remove_Extraneous_Decimal_Digit
     (Textual_Rep     : in out String;
      First_Non_Blank : in out Positive)
   is
      Point_Position : constant Positive := Ada.Strings.Fixed.Index
        (Textual_Rep,
         ".",
         First_Non_Blank);

      Integral_Part_Needs_Increment : constant Boolean :=
        Textual_Rep (Point_Position + 1) in '5' .. '9';
   begin
      Ada.Strings.Fixed.Delete
        (Textual_Rep,
         Point_Position,
         Point_Position + 1,
         Ada.Strings.Right);

      First_Non_Blank := First_Non_Blank + 2;

      if Integral_Part_Needs_Increment then
         Increment_Integral_Part
           (Textual_Rep,
            First_Non_Blank,
            Last_Digit_Position => Point_Position + 1);
      end if;
   end Remove_Extraneous_Decimal_Digit;

   --------------------------
   -- Trim_Fractional_Part --
   --------------------------

   procedure Trim_Fractional_Part
     (Textual_Rep     : in out String;
      First_Non_Blank : in out Positive)
   is
      Cursor : Positive :=
        Ada.Strings.Fixed.Index (Textual_Rep, ".", First_Non_Blank);

      First_To_Trim : Positive;
      Fractional_Part_Last : Positive;
   begin
      while Cursor + 1 <= Textual_Rep'Last
        and then Textual_Rep (Cursor + 1) in '0' .. '9' loop
         Cursor := Cursor + 1;
      end loop;

      Fractional_Part_Last := Cursor;

      while Textual_Rep (Cursor) = '0' loop
         Cursor := Cursor - 1;
      end loop;

      if Textual_Rep (Cursor) = '.' then
         Cursor := Cursor - 1;
      end if;

      First_To_Trim := Cursor + 1;

      Ada.Strings.Fixed.Delete
        (Textual_Rep, First_To_Trim, Fractional_Part_Last, Ada.Strings.Right);

      First_Non_Blank :=
        First_Non_Blank + (Fractional_Part_Last - First_To_Trim + 1);
   end Trim_Fractional_Part;

end GNAT.Formatted_String;
