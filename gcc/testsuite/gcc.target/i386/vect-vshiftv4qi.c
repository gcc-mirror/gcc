/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */

#define N 4

typedef unsigned char __vu __attribute__ ((__vector_size__ (N)));
typedef signed char __vi __attribute__ ((__vector_size__ (N)));

__vu sllv (__vu a, __vu b)
{
  return a << b;
}

/* { dg-final { scan-assembler-times "vpsllvw" 1 } } */

__vu srlv (__vu a, __vu b)
{
  return a >> b;
}

/* { dg-final { scan-assembler-times "vpsrlvw" 1 } } */

__vi srav (__vi a, __vi b)
{
  return a >> b;
}

/* { dg-final { scan-assembler-times "vpsravw" 1 } } */
