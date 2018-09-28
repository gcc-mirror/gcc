/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate -fprofile-use -fopt-info-missed-ipa -Wno-missing-profile" } */
/* { dg-require-profiling "-fprofile-generate" } */

unsigned test (unsigned a, unsigned b)
{
  return a / b;
} /* { dg-missed "\[^\n\]*execution counts estimated" } */
