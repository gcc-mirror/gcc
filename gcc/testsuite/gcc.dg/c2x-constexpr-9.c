/* Test C2x constexpr.  Invalid code, compilation tests, IEEE arithmetic.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target inff } */

/* A conversion from signaling NaN to quiet NaN in a different format or type
   is not valid for constexpr.  */
constexpr float fns = __builtin_nans (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr double dns = __builtin_nansf (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr long double ldns = __builtin_nans (""); /* { dg-error "'constexpr' initializer not representable in type of object" } */

/* Test out-of-range values.  */
constexpr float fu = __DBL_MIN__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr float fo = __DBL_MAX__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr float fp = 0x1.ffffffp0; /* { dg-error "'constexpr' initializer not representable in type of object" } */

constexpr _Complex float cfur = __builtin_complex (__DBL_MIN__, 0.0); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Complex float cfor = __builtin_complex (__DBL_MAX__, 0.0); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Complex float cfpr = __builtin_complex (0x1.ffffffp0, 0.0); /* { dg-error "'constexpr' initializer not representable in type of object" } */

constexpr _Complex float cfui = __builtin_complex (0.0, __DBL_MIN__); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Complex float cfoi = __builtin_complex (0.0, __DBL_MAX__); /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Complex float cfpi = __builtin_complex (0.0, 0x1.ffffffp0); /* { dg-error "'constexpr' initializer not representable in type of object" } */

constexpr _Complex float cfd = __DBL_MAX__; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr _Complex float cfi = __LONG_LONG_MAX__; /* { dg-error "'constexpr' initializer not representable in type of object" } */

void
f0 ()
{
  (constexpr float) { __builtin_nans ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr double) { __builtin_nansf ("") }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr float) { __DBL_MIN__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr float) { __DBL_MAX__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr float) { 0x1.ffffffp0 }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __builtin_complex (__DBL_MIN__, 0.0) }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __builtin_complex (__DBL_MAX__, 0.0) }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __builtin_complex (0x1.ffffffp0, 0.0) }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __builtin_complex (0.0, __DBL_MIN__) }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __builtin_complex (0.0, __DBL_MAX__) }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __builtin_complex (0.0, 0x1.ffffffp0) }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __DBL_MAX__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
  (constexpr _Complex float) { __LONG_LONG_MAX__ }; /* { dg-error "'constexpr' initializer not representable in type of object" } */
}
