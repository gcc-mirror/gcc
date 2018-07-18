/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx -mtune=generic" } */
/* { dg-skip-if "" { x86_64-*-mingw* } } */
/* { dg-final { scan-assembler "and\[lq\]?\[\\t \]*\\$-32,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler "vmovaps\[\\t \]*%ymm" } } */

extern void abort (void);

static float Yf[] = { 2.0, -2.0, -2.0, -2.0, -2.0, 2.0, -0.0, __builtin_inff () };
static const float Zf[] = { 1.0, -1.0, -1.0, -0.0, -0.0, 0.0, -__builtin_inff (), __builtin_nanf ("") };

void testf (void)
{
  float xxxxx[8];
  int i;
  xxxxx[0] = __builtin_copysignf (1.0, Yf[0]);
  xxxxx[1] = __builtin_copysignf (1.0, Yf[1]);
  xxxxx[2] = __builtin_copysignf (-1.0, Yf[2]);
  xxxxx[3] = __builtin_copysignf (0.0, Yf[3]);
  xxxxx[4] = __builtin_copysignf (-0.0, Yf[4]);
  xxxxx[5] = __builtin_copysignf (-0.0, Yf[5]);
  xxxxx[6] = __builtin_copysignf (__builtin_inff (), Yf[6]);
  xxxxx[7] = __builtin_copysignf (-__builtin_nanf (""), Yf[7]);
  for (i = 0; i < 8; ++i)
    if (__builtin_memcmp (xxxxx+i, Zf+i, sizeof(float)) != 0)
      abort ();
}
