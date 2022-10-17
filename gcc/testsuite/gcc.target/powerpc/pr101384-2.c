/* PR target/101384 */
/* { dg-do compile { target be } } */
/* { dg-options "-O2 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-final { scan-assembler-times {\mvspltis[whb] [^\n\r]*,-1\M} 9 } } */
/* { dg-final { scan-assembler-times {\mvslw\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvslh\M} 3 } } */
/* { dg-final { scan-assembler-times {\mvslb\M} 3 } } */

typedef unsigned char __attribute__((__vector_size__ (16))) U;
typedef unsigned short __attribute__((__vector_size__ (16))) V;
typedef unsigned int __attribute__((__vector_size__ (16))) W;

U u;
V v;
W w;

U
f1 (void)
{
  U y = (U) { 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 } + u;
  return y;
}

U
f2 (void)
{
  U y = (U) { 0x80, 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80, 0, 0x80, 0 } + u;
  return y;
}

U
f3 (void)
{
  U y = (U) { 0x80, 0, 0, 0, 0x80, 0, 0, 0, 0x80, 0, 0, 0, 0x80, 0, 0, 0 } + u;
  return y;
}

V
f4 (void)
{
  V y = (V) { 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080, 0x8080 } + v;
  return y;
}

V
f5 (void)
{
  V y = (V) { 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000 } + v;
  return y;
}

V
f6 (void)
{
  V y = (V) { 0x8000, 0, 0x8000, 0, 0x8000, 0, 0x8000, 0 } + v;
  return y;
}

W
f7 (void)
{
  W y = (W) { 0x80808080, 0x80808080, 0x80808080, 0x80808080 } + w;
  return y;
}

W
f8 (void)
{
  W y = (W) { 0x80008000, 0x80008000, 0x80008000, 0x80008000 } + w;
  return y;
}

W
f9 (void)
{
  W y = (W) { 0x80000000, 0x80000000, 0x80000000, 0x80000000 } + w;
  return y;
}
