/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */

typedef long long __m256i __attribute__ ((__vector_size__ (32)));

extern void ext (void);

void foo (__m256i x, __m256i y)
{
  __m256i a = x & ~y;
  if (__builtin_ia32_ptestz256 (a, a))
    ext();
}

void bar (__m256i x, __m256i y)
{
  __m256i a = ~x & y;
  if (__builtin_ia32_ptestz256 (a, a))
    ext();
}

void foo2 (__m256i x, __m256i y)
{
  __m256i a = x & ~y;
  if (__builtin_ia32_ptestz256 (a, a))
    ext();
}

void bar2 (__m256i x, __m256i y)
{
  __m256i a = ~x & y;
  if (__builtin_ia32_ptestz256 (a, a))
    ext();
}

/* { dg-final { scan-assembler-times "ptest\[ \\t\]+%" 4 } } */
/* { dg-final { scan-assembler-times "jn?c" 4 } } */
/* { dg-final { scan-assembler-not "pandn" } } */
/* { dg-final { scan-assembler-not "jne" } } */
/* { dg-final { scan-assembler-not "je" } } */
