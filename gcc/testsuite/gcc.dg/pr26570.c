/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate -fprofile-use -fopt-info -Wno-missing-profile" } */
/* { dg-require-profiling "-fprofile-generate" } */

unsigned test (unsigned a, unsigned b)
{
  return a / b;
} /* { dg-message "note: \[^\n\]*execution counts estimated" } */
