/* Test C2x constexpr.  Invalid code, compilation tests, DFP.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

/* Test conversions between binary and decimal.  */
constexpr double v2 = 0.0DF; /* { dg-error "'constexpr' initializer for a binary floating-point type is of decimal type" } */
constexpr double v2i = __builtin_infd32 (); /* { dg-error "'constexpr' initializer for a binary floating-point type is of decimal type" } */
constexpr double v2n = __builtin_nand32 (""); /* { dg-error "'constexpr' initializer for a binary floating-point type is of decimal type" } */
constexpr _Decimal128 v2d = 0x1p-100f; /* { dg-error "'constexpr' initializer not representable in type of object" } */

/* A conversion from signaling NaN to quiet NaN in a different format is not
   valid for constexpr.  */
constexpr _Decimal32 v3 = __builtin_nansd64 (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal32 v4 = __builtin_nansd128 (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal64 v5 = __builtin_nansd32 (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal64 v6 = __builtin_nansd128 (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal128 v7 = __builtin_nansd32 (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal128 v8 = __builtin_nansd64 (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */

/* Test out-of-range values, including integers.  */
constexpr _Decimal32 v9 = 12345678; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal64 v10 = (unsigned long long) -1; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal32 v11 = __DEC64_MIN__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal64 v12 = -__DEC128_MAX__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal64 v13 = 12345678901234567890.DL; /* { dg-error "'constexpr' initializer not representable in type of object" } */

/* Test cases where the value can be represented, but the quantum exponent
   cannot.  */
constexpr _Decimal32 v14 = 0e-200DD; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Decimal32 v15 = 0e200DL; /* { dg-error "'constexpr' initializer not representable in type of object" } */

void
f0 ()
{
  (constexpr double) { 0.0DF }; /* { dg-error "'constexpr' initializer for a binary floating-point type is of decimal type" } */
  (constexpr double) { __builtin_infd32 () }; /* { dg-error "'constexpr' initializer for a binary floating-point type is of decimal type" } */
  (constexpr double) { __builtin_nand32 ("") }; /* { dg-error "'constexpr' initializer for a binary floating-point type is of decimal type" } */
  (constexpr _Decimal128) { 0x1p-100f }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal32) { __builtin_nansd64 ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal32) { __builtin_nansd128 ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal64) { __builtin_nansd32 ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal64) { __builtin_nansd128 ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal128) { __builtin_nansd32 ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal128) { __builtin_nansd64 ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal32) { 12345678 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal64) { (unsigned long long) -1 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal32) { __DEC64_MIN__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal64) { -__DEC128_MAX__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal64) { 12345678901234567890.DL }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal32) { 0e-200DD }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Decimal32) { 0e200DL }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
}
