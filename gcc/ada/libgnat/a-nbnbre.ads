------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   ADA.NUMERICS.BIG_NUMBERS.BIG_REALS                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Strings.Text_Buffers; use Ada.Strings.Text_Buffers;

package Ada.Numerics.Big_Numbers.Big_Reals
  with Preelaborate
is
   type Big_Real is private with
     Real_Literal => From_Universal_Image,
     Put_Image    => Put_Image;

   function Is_Valid (Arg : Big_Real) return Boolean
   with
     Convention => Intrinsic,
     Global     => null;

   subtype Valid_Big_Real is Big_Real
     with Dynamic_Predicate => Is_Valid (Valid_Big_Real),
          Predicate_Failure => raise Program_Error;

   function "/"
     (Num, Den : Big_Integers.Valid_Big_Integer) return Valid_Big_Real
     with Global => null;
--   with Pre => (Big_Integers."/=" (Den, Big_Integers.To_Big_Integer (0))
--                or else Constraint_Error);

   function Numerator
     (Arg : Valid_Big_Real) return Big_Integers.Valid_Big_Integer
     with Global => null;

   function Denominator (Arg : Valid_Big_Real) return Big_Integers.Big_Positive
   with
     Post   =>
       (if Arg = To_Real (0)
        then Big_Integers."=" (Denominator'Result,
                               Big_Integers.To_Big_Integer (1))
        else Big_Integers."="
               (Big_Integers.Greatest_Common_Divisor
                 (Numerator (Arg), Denominator'Result),
                Big_Integers.To_Big_Integer (1))),
     Global => null;

   function To_Big_Real
     (Arg : Big_Integers.Big_Integer)
     return Valid_Big_Real is (Arg / Big_Integers.To_Big_Integer (1))
     with Global => null;

   function To_Real (Arg : Integer) return Valid_Big_Real is
     (Big_Integers.To_Big_Integer (Arg) / Big_Integers.To_Big_Integer (1))
     with Global => null;

   function "=" (L, R : Valid_Big_Real) return Boolean with Global => null;

   function "<" (L, R : Valid_Big_Real) return Boolean with Global => null;

   function "<=" (L, R : Valid_Big_Real) return Boolean with Global => null;

   function ">" (L, R : Valid_Big_Real) return Boolean with Global => null;

   function ">=" (L, R : Valid_Big_Real) return Boolean with Global => null;

   function In_Range (Arg, Low, High : Big_Real) return Boolean is
     (Low <= Arg and then Arg <= High)
     with Global => null;

   generic
      type Num is digits <>;
   package Float_Conversions is

      function To_Big_Real (Arg : Num) return Valid_Big_Real
        with Global => null;

      function From_Big_Real (Arg : Big_Real) return Num
      with
        Pre    => In_Range (Arg,
                            Low  => To_Big_Real (Num'First),
                            High => To_Big_Real (Num'Last))
                   or else (raise Constraint_Error),
        Global => null;

   end Float_Conversions;

   generic
      type Num is delta <>;
   package Fixed_Conversions is

      function To_Big_Real (Arg : Num) return Valid_Big_Real
        with Global => null;

      function From_Big_Real (Arg : Big_Real) return Num
      with
        Pre    => In_Range (Arg,
                            Low  => To_Big_Real (Num'First),
                            High => To_Big_Real (Num'Last))
                   or else (raise Constraint_Error),
        Global => null;

   end Fixed_Conversions;

   function To_String (Arg  : Valid_Big_Real;
                       Fore : Field := 2;
                       Aft  : Field := 3;
                       Exp  : Field := 0) return String
   with
     Post   => To_String'Result'First = 1,
     Global => null;

   function From_String (Arg : String) return Valid_Big_Real
     with Global => null;

   function From_Universal_Image (Arg : String) return Valid_Big_Real
     renames From_String;
   function From_Universal_Image (Num, Den : String) return Valid_Big_Real is
     (Big_Integers.From_Universal_Image (Num) /
      Big_Integers.From_Universal_Image (Den))
      with Global => null;

   function To_Quotient_String (Arg : Big_Real) return String is
     (Big_Integers.To_String (Numerator (Arg)) & " / "
      & Big_Integers.To_String (Denominator (Arg)))
     with Global => null;

   function From_Quotient_String (Arg : String) return Valid_Big_Real
     with Global => null;

   procedure Put_Image (S : in out Root_Buffer_Type'Class; V : Big_Real);

   function "+" (L : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "-" (L : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "abs" (L : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "+" (L, R : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "-" (L, R : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "*" (L, R : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "/" (L, R : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function "**" (L : Valid_Big_Real; R : Integer) return Valid_Big_Real
     with Global => null;

   function Min (L, R : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

   function Max (L, R : Valid_Big_Real) return Valid_Big_Real
     with Global => null;

private

   type Big_Real is record
      Num, Den : Big_Integers.Big_Integer;
   end record;

end Ada.Numerics.Big_Numbers.Big_Reals;
