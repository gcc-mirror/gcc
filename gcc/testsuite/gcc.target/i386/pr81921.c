/* { dg-do compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -march=x86-64" } */

extern __inline int  __attribute__((__gnu_inline__, __always_inline__, __artificial__, target("sse2")))
_mm_loadu_si128 (int const *__P)
{
    return *__P;
}

void __attribute__((target("ssse3"))) foo (void *p)
{
  volatile int x = _mm_loadu_si128 (p);
}
