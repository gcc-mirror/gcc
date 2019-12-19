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
with Ada.Streams;

--  Note that some Ada 2020 aspects are commented out since they are not
--  supported yet.

package Ada.Numerics.Big_Numbers.Big_Reals
  with Preelaborate
--  Nonblocking, Global => in out synchronized Big_Reals
is
   type Big_Real is private;
--   with Real_Literal => From_String,
--        Put_Image    => Put_Image;

   function Is_Valid (Arg : Big_Real) return Boolean;

   function "/" (Num, Den : Big_Integers.Big_Integer) return Big_Real;
--   with Pre => (if Big_Integers."=" (Den, Big_Integers.To_Big_Integer (0))
--                then raise Constraint_Error);

   function Numerator (Arg : Big_Real) return Big_Integers.Big_Integer;

   function Denominator (Arg : Big_Real) return Big_Integers.Big_Positive
     with Post =>
       (Arg = To_Real (0)) or else
       (Big_Integers."="
         (Big_Integers.Greatest_Common_Divisor
           (Numerator (Arg), Denominator'Result),
          Big_Integers.To_Big_Integer (1)));

   function To_Big_Real
     (Arg : Big_Integers.Big_Integer)
     return Big_Real is (Arg / Big_Integers.To_Big_Integer (1));

   function To_Real (Arg : Integer) return Big_Real is
     (Big_Integers.To_Big_Integer (Arg) / Big_Integers.To_Big_Integer (1));

   function "=" (L, R : Big_Real) return Boolean;

   function "<" (L, R : Big_Real) return Boolean;

   function "<=" (L, R : Big_Real) return Boolean;

   function ">" (L, R : Big_Real) return Boolean;

   function ">=" (L, R : Big_Real) return Boolean;

   function In_Range (Arg, Low, High : Big_Real) return Boolean is
     (Low <= Arg and then Arg <= High);

   generic
      type Num is digits <>;
   package Float_Conversions is

      function To_Big_Real (Arg : Num) return Big_Real;

      function From_Big_Real (Arg : Big_Real) return Num
        with Pre => In_Range (Arg,
                              Low  => To_Big_Real (Num'First),
                              High => To_Big_Real (Num'Last))
                    or else (raise Constraint_Error);

   end Float_Conversions;

   generic
      type Num is delta <>;
   package Fixed_Conversions is

      function To_Big_Real (Arg : Num) return Big_Real;

      function From_Big_Real (Arg : Big_Real) return Num
        with Pre => In_Range (Arg,
                              Low  => To_Big_Real (Num'First),
                              High => To_Big_Real (Num'Last))
                    or else (raise Constraint_Error);

   end Fixed_Conversions;

   function To_String (Arg  : Big_Real;
                       Fore : Field := 2;
                       Aft  : Field := 3;
                       Exp  : Field := 0) return String
      with Post => To_String'Result'First = 1;

   function From_String (Arg : String) return Big_Real;

   function To_Quotient_String (Arg : Big_Real) return String is
     (Big_Integers.To_String (Numerator (Arg)) & " / "
      & Big_Integers.To_String (Denominator (Arg)));

   function From_Quotient_String (Arg : String) return Big_Real;

   procedure Put_Image
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Arg    : Big_Real);

   function "+" (L : Big_Real) return Big_Real;

   function "-" (L : Big_Real) return Big_Real;

   function "abs" (L : Big_Real) return Big_Real;

   function "+" (L, R : Big_Real) return Big_Real;

   function "-" (L, R : Big_Real) return Big_Real;

   function "*" (L, R : Big_Real) return Big_Real;

   function "/" (L, R : Big_Real) return Big_Real;

   function "**" (L : Big_Real; R : Integer) return Big_Real;

   function Min (L, R : Big_Real) return Big_Real;

   function Max (L, R : Big_Real) return Big_Real;

private

   type Big_Real is record
      Num, Den : Big_Integers.Big_Integer;
   end record;

end Ada.Numerics.Big_Numbers.Big_Reals;
