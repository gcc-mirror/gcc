/* -Wall is not supposed to trigger -Wsign-compare for C.  PR 10604.
   See also g++.dg/warn/compare1.C.  */

/* { dg-do compile } */
/* { dg-options "-Wall" } */

int f(unsigned a, int b)
{
  return a < b;  /* { dg-bogus "signed and unsigned" } */
}
