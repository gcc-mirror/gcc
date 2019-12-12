/* { dg-do run } */
/* { dg-options "-save-temps -O2 -fno-inline" } */

#define FUNC_DEFS(__a)				\
float						\
fsfoo##__a (int x)				\
{						\
  return ((float) x)/(1lu << __a);		\
}						\
float						\
fusfoo##__a (unsigned int x)			\
{						\
  return ((float) x)/(1lu << __a);		\
}						\
float						\
fslfoo##__a (long long x)			\
{						\
  return ((float) x)/(1lu << __a);		\
}						\
float						\
fulfoo##__a (unsigned long long x)		\
{						\
  return ((float) x)/(1lu << __a);		\
}						\

#define FUNC_DEFD(__a)				\
double						\
dsfoo##__a (int x)				\
{						\
  return ((double) x)/(1lu << __a);		\
}						\
double						\
dusfoo##__a (unsigned int x)			\
{						\
  return ((double) x)/(1lu << __a);		\
}						\
double						\
dslfoo##__a (long long x)			\
{						\
  return ((double) x)/(1lu << __a);		\
}						\
double						\
dulfoo##__a (unsigned long long x)		\
{						\
  return ((double) x)/(1lu << __a);		\
}

FUNC_DEFS (4)
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], w\[0-9\]*.*#4" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], w\[0-9\]*.*#4" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], x\[0-9\]*.*#4" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], x\[0-9\]*.*#4" 1 } } */

FUNC_DEFD (4)
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], w\[0-9\]*.*#4" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], w\[0-9\]*.*#4" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], x\[0-9\]*.*#4" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], x\[0-9\]*.*#4" 1 } } */

FUNC_DEFS (8)
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], w\[0-9\]*.*#8" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], w\[0-9\]*.*#8" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], x\[0-9\]*.*#8" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], x\[0-9\]*.*#8" 1 } } */

FUNC_DEFD (8)
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], w\[0-9\]*.*#8" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], w\[0-9\]*.*#8" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], x\[0-9\]*.*#8" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], x\[0-9\]*.*#8" 1 } } */

FUNC_DEFS (16)
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], w\[0-9\]*.*#16" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], w\[0-9\]*.*#16" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], x\[0-9\]*.*#16" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], x\[0-9\]*.*#16" 1 } } */

FUNC_DEFD (16)
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], w\[0-9\]*.*#16" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], w\[0-9\]*.*#16" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], x\[0-9\]*.*#16" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], x\[0-9\]*.*#16" 1 } } */

FUNC_DEFS (32)
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], w\[0-9\]*.*#32" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], w\[0-9\]*.*#32" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\ts\[0-9\], x\[0-9\]*.*#32" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\ts\[0-9\], x\[0-9\]*.*#32" 1 } } */

FUNC_DEFD (32)
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], w\[0-9\]*.*#32" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], w\[0-9\]*.*#32" 1 } } */
	/* { dg-final { scan-assembler-times "scvtf\td\[0-9\], x\[0-9\]*.*#32" 1 } } */
	/* { dg-final { scan-assembler-times "ucvtf\td\[0-9\], x\[0-9\]*.*#32" 1 } } */

#define FUNC_TESTS(__a, __b)					\
do								\
{								\
  if (fsfoo##__a (__b) !=  ((int) i) * (1.0f/(1lu << __a)) )	\
    __builtin_abort ();						\
  if (fusfoo##__a (__b) != ((int) i) * (1.0f/(1lu << __a)) )	\
    __builtin_abort ();						\
  if (fslfoo##__a (__b) != ((int) i) * (1.0f/(1lu << __a)) )	\
    __builtin_abort ();						\
  if (fulfoo##__a (__b) != ((int) i) * (1.0f/(1lu << __a)) )	\
    __builtin_abort ();						\
} while (0)

#define FUNC_TESTD(__a, __b)					\
do								\
{								\
  if (dsfoo##__a (__b) !=  ((int) i) * (1.0d/(1lu << __a)) )	\
    __builtin_abort ();						\
  if (dusfoo##__a (__b) != ((int) i) * (1.0d/(1lu << __a)) )	\
    __builtin_abort ();						\
  if (dslfoo##__a (__b) != ((int) i) * (1.0d/(1lu << __a)) )	\
    __builtin_abort ();						\
  if (dulfoo##__a (__b) != ((int) i) * (1.0d/(1lu << __a)) )	\
    __builtin_abort ();						\
} while (0)

int
main (void)
{
	int i;

	for (i = 0; i < 32; i ++)
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
