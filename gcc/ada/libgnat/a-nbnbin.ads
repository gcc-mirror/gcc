------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.NUMERICS.BIG_NUMBERS.BIG_INTEGERS                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Streams;

private with System;

--  Note that some Ada 2020 aspects are commented out since they are not
--  supported yet.

package Ada.Numerics.Big_Numbers.Big_Integers
  with Preelaborate
--  Nonblocking
is
   type Big_Integer is private;
   --  with Integer_Literal => From_String,
   --       Put_Image => Put_Image;

   function Is_Valid (Arg : Big_Integer) return Boolean
     with Convention => Intrinsic;

   function "=" (L, R : Big_Integer) return Boolean;

   function "<" (L, R : Big_Integer) return Boolean;

   function "<=" (L, R : Big_Integer) return Boolean;

   function ">" (L, R : Big_Integer) return Boolean;

   function ">=" (L, R : Big_Integer) return Boolean;

   function To_Big_Integer (Arg : Integer) return Big_Integer;

   subtype Big_Positive is Big_Integer
     with Dynamic_Predicate => Big_Positive > To_Big_Integer (0),
          Predicate_Failure => (raise Constraint_Error);

   subtype Big_Natural is Big_Integer
     with Dynamic_Predicate => Big_Natural >= To_Big_Integer (0),
          Predicate_Failure => (raise Constraint_Error);

   function In_Range (Arg, Low, High : Big_Integer) return Boolean is
     ((Low <= Arg) and (Arg <= High));

   function To_Integer (Arg : Big_Integer) return Integer
     with Pre => In_Range (Arg,
                           Low  => To_Big_Integer (Integer'First),
                           High => To_Big_Integer (Integer'Last))
                  or else (raise Constraint_Error);

   generic
      type Int is range <>;
   package Signed_Conversions is

      function To_Big_Integer (Arg : Int) return Big_Integer;

      function From_Big_Integer (Arg : Big_Integer) return Int
        with Pre => In_Range (Arg,
                              Low  => To_Big_Integer (Int'First),
                              High => To_Big_Integer (Int'Last))
                     or else (raise Constraint_Error);

   end Signed_Conversions;

   generic
      type Int is mod <>;
   package Unsigned_Conversions is

      function To_Big_Integer (Arg : Int) return Big_Integer;

      function From_Big_Integer (Arg : Big_Integer) return Int
        with Pre => In_Range (Arg,
                              Low  => To_Big_Integer (Int'First),
                              High => To_Big_Integer (Int'Last))
                     or else (raise Constraint_Error);

   end Unsigned_Conversions;

   function To_String (Arg : Big_Integer;
                       Width : Field := 0;
                       Base  : Number_Base := 10) return String
     with Post => To_String'Result'First = 1;

   function From_String (Arg : String) return Big_Integer;

   procedure Put_Image
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Arg    : Big_Integer);

   function "+" (L : Big_Integer) return Big_Integer;

   function "-" (L : Big_Integer) return Big_Integer;

   function "abs" (L : Big_Integer) return Big_Integer;

   function "+" (L, R : Big_Integer) return Big_Integer;

   function "-" (L, R : Big_Integer) return Big_Integer;

   function "*" (L, R : Big_Integer) return Big_Integer;

   function "/" (L, R : Big_Integer) return Big_Integer;

   function "mod" (L, R : Big_Integer) return Big_Integer;

   function "rem" (L, R : Big_Integer) return Big_Integer;

   function "**" (L : Big_Integer; R : Natural) return Big_Integer;

   function Min (L, R : Big_Integer) return Big_Integer;

   function Max (L, R : Big_Integer) return Big_Integer;

   function Greatest_Common_Divisor
     (L, R : Big_Integer) return Big_Positive
     with Pre => (L /= To_Big_Integer (0) and R /= To_Big_Integer (0))
       or else (raise Constraint_Error);

private

   type Controlled_Bignum is new Ada.Finalization.Controlled with record
      C : System.Address := System.Null_Address;
   end record;

   procedure Adjust   (This : in out Controlled_Bignum);
   procedure Finalize (This : in out Controlled_Bignum);

   type Big_Integer is record
      Value : Controlled_Bignum;
   end record;

end Ada.Numerics.Big_Numbers.Big_Integers;
