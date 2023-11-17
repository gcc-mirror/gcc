/* { dg-do run } */
/* { dg-options "-O2 -mlsx" } */
/* { dg-require-effective-target loongarch_sx_hw } */

extern void abort(void);

static double x = 2.0;
static float  y = 2.0;

int main()
{
  if ((2.5 * __builtin_copysign(1.0d, x)) != 2.5)
     abort();

  if ((2.5 * __builtin_copysign(1.0f, y)) != 2.5)
     abort();

  if ((2.5 * __builtin_copysignf(1.0d, -x)) != -2.5)
     abort();

  if ((2.5 * __builtin_copysignf(1.0f, -y)) != -2.5)
     abort();

  return 0;
}
