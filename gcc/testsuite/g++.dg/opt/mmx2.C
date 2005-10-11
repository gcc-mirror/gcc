// { dg-do link { target i?86-*-* x86_64-*-* } }
// { dg-options "-O2 -mmmx" }

#include <mmintrin.h>

static union u { __m64 m; long long l; } u;
extern "C" void abort (void);

__attribute__((noinline))
void bar (__m64 x)
{
  u.m = x;
}

int
main ()
{
  bar (_mm_set_pi32 (0x000000FF,0xFFFF00FF));
  _mm_empty ();
  if (u.l != 0xffffff00ffLL)
    abort ();
  return 0;
}
