/* Test C2x constexpr.  Valid code, compilation tests, IEEE arithmetic.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target inff } */

constexpr float fi = __builtin_inf ();
constexpr double di = __builtin_inff ();
constexpr float fn = __builtin_nan ("");
constexpr double dn = __builtin_nanf ("");
constexpr float fns = __builtin_nansf ("");
constexpr double dns = __builtin_nans ("");

void
f0 (void)
{
  (constexpr float) { __builtin_inf () };
  (constexpr double) { __builtin_inff () };
  (constexpr float) { __builtin_nan ("") };
  (constexpr double) { __builtin_nanf ("") };
  (constexpr float) { __builtin_nansf ("") };
  (constexpr double) { __builtin_nans ("") };
}
