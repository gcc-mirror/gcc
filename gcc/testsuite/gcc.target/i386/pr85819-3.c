/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mfpmath=sse" } */

typedef float To __attribute__ ((__vector_size__ (64)));
typedef unsigned int From __attribute__ ((__vector_size__ (64)));

#define A2(I) (float)a[I], (float)a[1+I]
#define A4(I) A2(I), A2(2+I)
#define A8(I) A4(I), A4(4+I)
#define A16(I) A8(I), A8(8+I)

To
f(From a)
{
  return __extension__ (To) {A16(0)};
}

/* { dg-final { scan-assembler "vcvtudq2ps" } } */
