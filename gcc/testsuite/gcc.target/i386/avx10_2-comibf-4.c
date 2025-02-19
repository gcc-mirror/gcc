/* { dg-do run { target { avx10_2_256 } } } */
/* { dg-options "-march=x86-64-v3 -O2" } */

#include "avx10_2-comibf-3.c"

__attribute__((noinline))
int foo1 (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a == b && c < d;
}

__attribute__((noinline))
int foo2 (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return a > b || c != d;
}

__attribute__((noinline))
int foo3 (__bf16 a, __bf16 b, __bf16 c, __bf16 d)
{
  return (a >= b) * (c <= d);
}


int main (void)
{
  if (!__builtin_cpu_supports ("avx10.2-256"))
    return 0;

  __bf16 a = 0.5bf16, b = -0.25bf16, c = 1.75bf16, d = -0.125bf16;

  if (foo1_avx10 (a, b, c, d) != foo1 (a, b, c, d))
    __builtin_abort ();

  if (foo2_avx10 (b, c, d, a) != foo2 (b, c, d, a))
    __builtin_abort ();
  
  if (foo3_avx10 (c, d, a, b) != foo3 (c, d, a, b))
    __builtin_abort ();
}

