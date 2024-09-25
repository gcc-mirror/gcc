/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z15" } */
/* { dg-require-effective-target s390_vxe2 } */
/* { dg-final { scan-assembler-times {\tvstbrq\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvster[hfg]\t} 5 } } */
/* { dg-final { scan-assembler-not {\tvperm\t} } } */

typedef signed char __attribute__ ((vector_size (16))) V16QI;
typedef short __attribute__ ((vector_size (16))) V8HI;
typedef int __attribute__ ((vector_size (16))) V4SI;
typedef long long __attribute__ ((vector_size (16))) V2DI;
typedef float __attribute__ ((vector_size (16))) V4SF;
typedef double __attribute__ ((vector_size (16))) V2DF;

void
v16qi (V16QI *x, V16QI y)
{
  V16QI z;
  for (int i = 0; i < 16; ++i)
    z[i] = y[15 - i];
  *x = z;
}

void
v8hi (V8HI *x, V8HI y)
{
  V8HI z;
  for (int i = 0; i < 8; ++i)
    z[i] = y[7 - i];
  *x = z;
}

void
v4si (V4SI *x, V4SI y)
{
  V4SI z;
  for (int i = 0; i < 4; ++i)
    z[i] = y[3 - i];
  *x = z;
}

void
v2di (V2DI *x, V2DI y)
{
  V2DI z;
  for (int i = 0; i < 2; ++i)
    z[i] = y[1 - i];
  *x = z;
}

void
v4sf (V4SF *x, V4SF y)
{
  V4SF z;
  for (int i = 0; i < 4; ++i)
    z[i] = y[3 - i];
  *x = z;
}

void
v2df (V2DF *x, V2DF y)
{
  V2DF z;
  for (int i = 0; i < 2; ++i)
    z[i] = y[1 - i];
  *x = z;
}
