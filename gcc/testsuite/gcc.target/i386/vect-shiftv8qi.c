/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-avx2 -mno-avx512vl" } */

#define N 8

typedef unsigned char __vu __attribute__ ((__vector_size__ (N)));
typedef signed char __vi __attribute__ ((__vector_size__ (N)));

__vu sll (__vu a, int n)
{
  return a << n;
}

__vu sll_c (__vu a)
{
  return a << 5;
}

/* { dg-final { scan-assembler-times "psllw" 2 } } */

__vu srl (__vu a, int n)
{
  return a >> n;
}

__vu srl_c (__vu a)
{
  return a >> 5;
}

/* { dg-final { scan-assembler-times "psrlw" 5 } } */

__vi sra (__vi a, int n)
{
  return a >> n;
}

__vi sra_c (__vi a)
{
  return a >> 5;
}

/* { dg-final { scan-assembler-times "psraw" 2 } } */
