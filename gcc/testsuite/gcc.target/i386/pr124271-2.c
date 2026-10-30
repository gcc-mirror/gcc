/* PR target/124271 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mno-avx512dq" } */

typedef unsigned long long v2du __attribute__ ((vector_size (16)));
typedef unsigned long long v4du __attribute__ ((vector_size (32)));
typedef unsigned long long v8du __attribute__ ((vector_size (64)));
typedef long long v2di __attribute__ ((vector_size (16)));

#define M 0xffffffffULL

v2du
f1 (v2du x, v2du y)
{
  return (x & M) * (y & M);
}

v4du
f2 (v4du x, v4du y)
{
  return (x & M) * (y & M);
}

v8du
f3 (v8du x, v8du y)
{
  return (x & M) * (y & M);
}

v2di
f4 (v2di x, v2di y)
{
  return (x & (long long) M) * (y & (long long) M);
}

/* { dg-final { scan-assembler-times "vpmuludq\[ \\t\]+\[^\n\]*%xmm" 2 } } */
/* { dg-final { scan-assembler-times "vpmuludq\[ \\t\]+\[^\n\]*%ymm" 1 } } */
/* { dg-final { scan-assembler-times "vpmuludq\[ \\t\]+\[^\n\]*%zmm" 1 } } */
/* { dg-final { scan-assembler-not "vpsrlq" } } */
/* { dg-final { scan-assembler-not "vpsllq" } } */
