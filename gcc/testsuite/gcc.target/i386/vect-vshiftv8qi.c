/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

#define N 8

typedef unsigned char __vu __attribute__ ((__vector_size__ (N)));
typedef signed char __vi __attribute__ ((__vector_size__ (N)));

__vu vsll (__vu a, __vu b)
{
  return a << b;
}

/* { dg-final { scan-assembler-times "vpsllvw" 1 } } */

__vu vsrl (__vu a, __vu b)
{
  return a >> b;
}

/* { dg-final { scan-assembler-times "vpsrlvw" 1 } } */

__vi vsra (__vi a, __vi b)
{
  return a >> b;
}

/* { dg-final { scan-assembler-times "vpsravw" 1 } } */
