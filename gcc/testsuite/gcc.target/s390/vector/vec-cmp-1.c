/* Check that the proper unsigned compare instructions are being generated.  */

/* { dg-do compile { target { s390*-*-* } } } */
/* { dg-options "-O3 -mzarch -march=z13" } */

/* { dg-final { scan-assembler-times "vchlb" 1 } } */
/* { dg-final { scan-assembler-times "vchlh" 1 } } */
/* { dg-final { scan-assembler-times "vchlf" 1 } } */
/* { dg-final { scan-assembler-times "vchlg" 1 } } */

typedef __attribute__((vector_size(16))) signed char v16qi;
typedef __attribute__((vector_size(16))) unsigned char uv16qi;

typedef __attribute__((vector_size(16))) signed short v8hi;
typedef __attribute__((vector_size(16))) unsigned short uv8hi;

typedef __attribute__((vector_size(16))) signed int v4si;
typedef __attribute__((vector_size(16))) unsigned int uv4si;

typedef __attribute__((vector_size(16))) signed long long v2di;
typedef __attribute__((vector_size(16))) unsigned long long uv2di;

v16qi
f (uv16qi a, uv16qi b)
{
  return a > b;
}

v8hi
g (uv8hi a, uv8hi b)
{
  return a > b;
}

v4si
h (uv4si a, uv4si b)
{
  return a > b;
}

v2di
i (uv2di a, uv2di b)
{
  return a > b;
}
