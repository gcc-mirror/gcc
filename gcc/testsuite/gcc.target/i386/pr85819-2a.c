/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -mavx2 -mfma -mfpmath=sse" } */

typedef float To __attribute__ ((__vector_size__ (32)));
typedef unsigned int From __attribute__ ((__vector_size__ (32)));

#define A2(I) (float)a[I], (float)a[1+I]
#define A4(I) A2(I), A2(2+I)
#define A8(I) A4(I), A4(4+I)

To
f(From a)
{
  return __extension__ (To) {A8(0)};
}

/* { dg-final { scan-assembler "vfmadd132ps" } } */
