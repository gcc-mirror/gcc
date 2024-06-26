/* { dg-do compile } */
/* { dg-options "-O2 -msse4.2 -mno-avx -fno-tree-slp-vectorize" } */

void foo (int * __restrict a, int * __restrict b, int *c, int s)
{
  if (s >= 2)
    for (int i = 0; i < 1024; ++i)
      {
	a[s*i+0] = c[4*i+0];
	a[s*i+1] = c[4*i+1];
	b[s*i+0] = c[4*i+2];
	b[s*i+1] = c[4*i+3];
      }
}

/* Vectorization factor two, two two-element stores to a using movq
   and two two-element stores to b via pextrq/movhps of the high part.  */
/* { dg-final { scan-assembler-times "movq\[\t ]+%xmm\[0-9]" 2 } } */
/* { dg-final { scan-assembler-times "pextrq" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movhps" 2 { target { ia32 } } } } */
