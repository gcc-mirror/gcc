/* Test C2x constexpr.  Invalid code, compilation tests, excess precision.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -mfpmath=387 -fexcess-precision=standard" } */

constexpr double d = 1.0 / 3.0; /* { dg-error "'constexpr' initializer not representable in type of object" } */
constexpr double d2 = 1.1; /* { dg-error "'constexpr' initializer not representable in type of object" } */
