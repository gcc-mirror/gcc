/* PR target/97249  */
/* { dg-do compile } */
/* { dg-options "-mavx2 -O3 -masm=att" } */
/* { dg-final { scan-assembler-times {(?n)vpmovzxbw[ \t]+\(.*%xmm[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vpmovzxwd[ \t]+\(.*%xmm[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {(?n)vpmovzxdq[ \t]+\(.*%xmm[0-9]} 2 } } */

void
foo (unsigned char* p1, unsigned char* p2, short* __restrict p3)
{
  /* Avoid loop vectorization.  */
#pragma GCC unroll 8
  for (int i = 0 ; i != 8; i++)
    p3[i] = p1[i] + p2[i];
}

void
foo1 (unsigned short* p1, unsigned short* p2, int* __restrict p3)
{
  /* Avoid loop vectorization.  */
#pragma GCC unroll 4
  for (int i = 0 ; i != 4; i++)
    p3[i] = p1[i] + p2[i];
}

void
foo2 (unsigned int* p1, unsigned int* p2, long long* __restrict p3)
{
  /* Avoid loop vectorization.  */
#pragma GCC unroll 2
  for (int i = 0 ; i != 2; i++)
    p3[i] = (long long)p1[i] + (long long)p2[i];
}
