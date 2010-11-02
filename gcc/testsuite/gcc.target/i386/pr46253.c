/* { dg-do compile } */
/* { dg-options "-O -g -mf16c -mtune=generic -dp" } */

typedef __m256i __attribute__ ((__vector_size__ (32)));

__m256i bar (void);
void foo (void)
{
  int i = 0;
  bar ();
  __builtin_ia32_vzeroupper ();
  while (++i);
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 1 } } */
