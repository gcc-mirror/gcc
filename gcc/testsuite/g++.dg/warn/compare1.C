/* -Wall is supposed to trigger -Wsign-compare for C++.  PR 10604.
   See also gcc.dg/compare7.c.  */

/* { dg-do compile } */
/* { dg-options "-Wall" } */

int f(unsigned a, int b)
{
  return a < b;  /* { dg-warning "signed and unsigned" } */
}
