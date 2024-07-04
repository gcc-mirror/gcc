/* PR tree-optimization/112774 */
/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O2 -ftree-vectorize" } */

int A[1024 * 2];

int foo (unsigned offset, unsigned N)
{
  int sum = 0;

  for (unsigned i = 0; i < N; i++)
    sum += A[i + offset];

  return sum;
}

/* Loop can be vectorized by referring "i + offset" is nonwrapping from array.  */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { ! { avr-*-* msp430-*-* pru-*-* } } } } } */
