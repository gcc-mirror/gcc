/* { dg-do compile } */
/* { dg-final { scan-assembler {\tvstbrh\t} } } */
/* { dg-final { scan-assembler {\tvstbrf\t} } } */
/* { dg-final { scan-assembler {\tvstbrg\t} } } */
/* { dg-final { scan-assembler-not {\tvperm\t} } } */

typedef unsigned short __attribute__ ((vector_size (16))) V8HI;
typedef unsigned int __attribute__ ((vector_size (16))) V4SI;
typedef unsigned long long __attribute__ ((vector_size (16))) V2DI;

void
vstbrh (V8HI *p, V8HI x)
{
  V8HI y;

  for (int i = 0; i < 8; ++i)
    y[i] = __builtin_bswap16 (x[i]);

  *p = y;
}

void
vstbrf (V4SI *p, V4SI x)
{
  V4SI y;

  for (int i = 0; i < 4; ++i)
    y[i] = __builtin_bswap32 (x[i]);

  *p = y;
}

void
vstbrg (V2DI *p, V2DI x)
{
  V2DI y;

  for (int i = 0; i < 2; ++i)
    y[i] = __builtin_bswap64 (x[i]);

  *p = y;
}
