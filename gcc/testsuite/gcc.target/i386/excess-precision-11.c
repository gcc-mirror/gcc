/* Test C2x constexpr.  Valid code, compilation tests, excess precision.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -mfpmath=387 -fexcess-precision=standard" } */

constexpr long double ld = 1.0 / 3.0;
constexpr long double ld2 = 1.1;
constexpr double d = (double) (1.0 / 3.0);
constexpr double d2 = (double) 1.1;
