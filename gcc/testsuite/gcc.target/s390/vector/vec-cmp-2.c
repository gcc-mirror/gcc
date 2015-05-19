/* Check that the proper signed compare instructions are being generated.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "vchb" 1 } } */
/* { dg-final { scan-assembler-times "vchh" 1 } } */
/* { dg-final { scan-assembler-times "vchf" 1 } } */
/* { dg-final { scan-assembler-times "vchg" 1 } } */

typedef __attribute__((vector_size(16))) signed char v16qi;
typedef __attribute__((vector_size(16))) signed short v8hi;
typedef __attribute__((vector_size(16))) signed int v4si;
typedef __attribute__((vector_size(16))) signed long long v2di;

v16qi
f (v16qi a, v16qi b)
{
  return a > b;
}

v8hi
g (v8hi a, v8hi b)
{
  return a > b;
}

v4si
h (v4si a, v4si b)
{
  return a > b;
}

v2di
i (v2di a, v2di b)
{
  return a > b;
}
