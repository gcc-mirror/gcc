/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

void
foo (float *buf)
{
  for (unsigned long i = 0, j = 100; i < 100; i++, j--)
    buf[i] = j;
}
