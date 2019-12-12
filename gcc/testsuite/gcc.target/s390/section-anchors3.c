/* Test corner case when LG from literal pool could be preferred to LARL.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O3 -march=z13" } */

int
a (int b)
{
  return b / 100;
  /* { dg-final { scan-assembler-not {\n\t\.quad\t\.LC} } } */
}
