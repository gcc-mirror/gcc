/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16)));

extern void ext (void);

void foo (__m128i x, __m128i y)
{
  __m128i a = x & ~y;
  if (__builtin_ia32_ptestz128 (a, a))
    ext();
}

void bar (__m128i x, __m128i y)
{
  __m128i a = ~x & y;
  if (__builtin_ia32_ptestz128 (a, a))
    ext();
}

void foo2 (__m128i x, __m128i y)
{
  __m128i a = x & ~y;
  if (__builtin_ia32_ptestz128 (a, a))
    ext();
}

void bar2 (__m128i x, __m128i y)
{
  __m128i a = ~x & y;
  if (__builtin_ia32_ptestz128 (a, a))
    ext();
}

/* { dg-final { scan-assembler-times "ptest\[ \\t\]+%" 4 } } */
/* { dg-final { scan-assembler-times "jn?c" 4 } } */
/* { dg-final { scan-assembler-not "pandn" } } */
/* { dg-final { scan-assembler-not "jne" } } */
/* { dg-final { scan-assembler-not "je" } } */
