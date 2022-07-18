------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               ADA.NUMERICS.BIG_NUMBERS.BIG_INTEGERS_GHOST                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a reduced and non-executable implementation of the
--  ARM A.5.6 defined ``Ada.Numerics.Big_Numbers.Big_Integers``  for use in
--  SPARK proofs in the runtime. As it is only intended for SPARK proofs, this
--  package is marked as a Ghost package and consequently does not have a
--  runtime footprint.

--  Contrary to Ada.Numerics.Big_Numbers.Big_Integers, this unit does not
--  depend on System or Ada.Finalization, which makes it more convenient for
--  use in run-time units. Note, since it is a ghost unit, all subprograms are
--  marked as imported.

--  Ghost code in this unit is meant for analysis only, not for run-time
--  checking. This is enforced by setting the assertion policy to Ignore.

pragma Assertion_Policy (Ghost => Ignore);

package Ada.Numerics.Big_Numbers.Big_Integers_Ghost with
   SPARK_Mode,
   Ghost,
   Pure,
   Always_Terminates
is

   type Big_Integer is private
     with Integer_Literal => From_Universal_Image;
   --  Private type that holds the integer value

   function Is_Valid (Arg : Big_Integer) return Boolean
   with
     Import,
     Global => null;
   --  Return whether a passed big integer is valid

   subtype Valid_Big_Integer is Big_Integer
     with Dynamic_Predicate => Is_Valid (Valid_Big_Integer),
          Predicate_Failure => raise Program_Error;
   --  Holds a valid Big_Integer

   --  Comparison operators defined for valid Big_Integer values
   function "=" (L, R : Valid_Big_Integer) return Boolean with
      Import,
      Global => null;

   function "<" (L, R : Valid_Big_Integer) return Boolean with
      Import,
      Global => null;

   function "<=" (L, R : Valid_Big_Integer) return Boolean with
      Import,
      Global => null;

   function ">" (L, R : Valid_Big_Integer) return Boolean with
      Import,
      Global => null;

   function ">=" (L, R : Valid_Big_Integer) return Boolean with
      Import,
      Global => null;

   function To_Big_Integer (Arg : Integer) return Valid_Big_Integer
     with
       Import,
       Global => null;
   --  Create a Big_Integer from an Integer value

   subtype Big_Positive is Big_Integer
     with Dynamic_Predicate =>
            (if Is_Valid (Big_Positive)
             then Big_Positive > To_Big_Integer (0)),
          Predicate_Failure => raise Constraint_Error;
   --  Positive subtype of Big_Integers, analogous to Positive and Integer

   subtype Big_Natural is Big_Integer
     with Dynamic_Predicate =>
            (if Is_Valid (Big_Natural)
             then Big_Natural >= To_Big_Integer (0)),
          Predicate_Failure => raise Constraint_Error;
   --  Natural subtype of Big_Integers, analogous to Natural and Integer

   function In_Range
     (Arg : Valid_Big_Integer; Low, High : Big_Integer) return Boolean
   is (Low <= Arg and Arg <= High)
   with
     Import,
     Global => null;
   --  Check whether Arg is in the range Low .. High

   function To_Integer (Arg : Valid_Big_Integer) return Integer
   with
     Import,
     Pre    => In_Range (Arg,
                         Low  => To_Big_Integer (Integer'First),
                         High => To_Big_Integer (Integer'Last))
                or else raise Constraint_Error,
     Global => null;
   --  Convert a valid Big_Integer into an Integer

   generic
      type Int is range <>;
   package Signed_Conversions is
      --  Generic package to implement conversion functions for
      --  arbitrary ranged types.

      function To_Big_Integer (Arg : Int) return Valid_Big_Integer
      with
        Global => null;
      --  Convert a ranged type into a valid Big_Integer

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int
      with
        Pre    => In_Range (Arg,
                            Low  => To_Big_Integer (Int'First),
                            High => To_Big_Integer (Int'Last))
                   or else raise Constraint_Error,
        Global => null;
      --  Convert a valid Big_Integer into a ranged type
   end Signed_Conversions;

   generic
      type Int is mod <>;
   package Unsigned_Conversions is
      --  Generic package to implement conversion functions for
      --  arbitrary modular types.

      function To_Big_Integer (Arg : Int) return Valid_Big_Integer
      with
        Global => null;
      --  Convert a modular type into a valid Big_Integer

      function From_Big_Integer (Arg : Valid_Big_Integer) return Int
      with
        Pre    => In_Range (Arg,
                            Low  => To_Big_Integer (Int'First),
                            High => To_Big_Integer (Int'Last))
                   or else raise Constraint_Error,
        Global => null;
      --  Convert a valid Big_Integer into a modular type

   end Unsigned_Conversions;

   function From_String (Arg : String) return Valid_Big_Integer
   with
     Import,
     Global => null;
   --  Create a valid Big_Integer from a String

   function From_Universal_Image (Arg : String) return Valid_Big_Integer
     renames From_String;

   --  Mathematical operators defined for valid Big_Integer values
   function "+" (L : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "-" (L : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "abs" (L : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "+" (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "-" (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "*" (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "/" (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "mod" (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "rem" (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function "**" (L : Valid_Big_Integer; R : Natural) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function Min (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function Max (L, R : Valid_Big_Integer) return Valid_Big_Integer
   with
     Import,
     Global => null;

   function Greatest_Common_Divisor
     (L, R : Valid_Big_Integer) return Big_Positive
   with
     Import,
     Pre    => (L /= To_Big_Integer (0) and R /= To_Big_Integer (0))
               or else raise Constraint_Error,
     Global => null;
   --  Calculate the greatest common divisor for two Big_Integer values

private
   pragma SPARK_Mode (Off);

   type Big_Integer is null record;
   --  Solely consists of Ghost code

end Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
