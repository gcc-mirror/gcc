/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-inline" } */

#define FUNC_DEFS(__a)	\
int			\
sffoo##__a (float x)	\
{			\
  return x * __a##.0f;	\
}			\
			\
unsigned int		\
usffoo##__a (float x)	\
{			\
  return x * __a##.0f;	\
}			\
			\
long			\
lsffoo##__a (float x)	\
{			\
  return x * __a##.0f;	\
}			\
			\
unsigned long		\
ulsffoo##__a (float x)	\
{			\
  return x * __a##.0f;	\
}

#define FUNC_DEFD(__a)	\
long long			\
dffoo##__a (double x)	\
{			\
  return x * __a##.0;	\
}			\
			\
unsigned long long	\
udffoo##__a (double x)	\
{			\
  return x * __a##.0;	\
}			\
int			\
sdffoo##__a (double x)	\
{			\
  return x * __a##.0;	\
}			\
			\
unsigned int		\
usdffoo##__a (double x)	\
{			\
  return x * __a##.0;	\
}

FUNC_DEFS (4)
FUNC_DEFD (4)
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], s\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], s\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], d\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], d\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], s\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], s\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], d\[0-9\]*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], d\[0-9\]*.*#2" 1 } } */

FUNC_DEFS (8)
FUNC_DEFD (8)
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], s\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], s\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], d\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], d\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], s\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], s\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], d\[0-9\]*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], d\[0-9\]*.*#3" 1 } } */

FUNC_DEFS (16)
FUNC_DEFD (16)
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], s\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], s\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], d\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], d\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], s\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], s\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], d\[0-9\]*.*#4" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], d\[0-9\]*.*#4" 1 } } */

FUNC_DEFS (32)
FUNC_DEFD (32)
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], s\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], s\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tx\[0-9\], d\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tw\[0-9\], d\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], s\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], s\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tx\[0-9\], d\[0-9\]*.*#5" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzu\tw\[0-9\], d\[0-9\]*.*#5" 1 } } */

#define FUNC_TESTS(__a, __b)					\
do								\
  {								\
    if (sffoo##__a (__b) != (int)(__b * __a))			\
      __builtin_abort ();					\
    if (usffoo##__a (__b) != (unsigned int)(__b * __a))	\
      __builtin_abort ();					\
    if (lsffoo##__a (__b) != (long long)(__b * __a))		\
      __builtin_abort ();					\
    if (ulsffoo##__a (__b) != (unsigned long long)(__b * __a))	\
      __builtin_abort ();					\
  } while (0)

#define FUNC_TESTD(__a, __b)					\
do								\
  {								\
    if (dffoo##__a (__b) != (long long)(__b * __a))		\
      __builtin_abort ();					\
    if (udffoo##__a (__b) != (unsigned long long)(__b * __a))	\
      __builtin_abort ();					\
    if (sdffoo##__a (__b) != (int)(__b * __a))			\
      __builtin_abort ();					\
    if (usdffoo##__a (__b) != (unsigned int)(__b * __a))	\
      __builtin_abort ();					\
  } while (0)

int
main (void)
{
  float i;

  for (i = -0.001; i < 32.0; i += 1.0f)
    {
      FUNC_TESTS (4, i);
      FUNC_TESTS (8, i);
      FUNC_TESTS (16, i);
      FUNC_TESTS (32, i);

      FUNC_TESTD (4, i);
      FUNC_TESTD (8, i);
      FUNC_TESTD (16, i);
      FUNC_TESTD (32, i);
    }
  return 0;
}
