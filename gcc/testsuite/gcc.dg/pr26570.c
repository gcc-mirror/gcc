/* { dg-do compile } */
/* { dg-options "-O2 -fprofile-generate -fprofile-use -fopt-info" } */
/* { dg-require-profiling "-fprofile-generate" } */

unsigned test (unsigned a, unsigned b)
{
  return a / b;
} /* { dg-message "note: \[^\n\]*execution counts estimated" } */
