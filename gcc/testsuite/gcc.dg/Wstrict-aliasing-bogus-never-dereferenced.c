/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-aliasing -fstrict-aliasing" } */


int foo ()
{
  int x = 10;
  int *p;
  float *q;

  q = (float*) &x;  /* { dg-bogus "not referenced" } */

  return x;
}
