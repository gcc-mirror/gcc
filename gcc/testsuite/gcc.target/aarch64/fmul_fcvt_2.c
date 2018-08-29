/* { dg-do run } */
/* { dg-options "-save-temps -O2 -ftree-vectorize -fno-inline -fno-vect-cost-model" } */

#pragma GCC target "+nosve"

#define N 1024

#define FUNC_DEF(__a)		\
void				\
foo##__a (float *a, int *b)	\
{				\
  int i;			\
  for (i = 0; i < N; i++)	\
    b[i] = a[i] * __a##.0f;	\
}

FUNC_DEF (4)
FUNC_DEF (8)
FUNC_DEF (16)

int ints[N];
float floats[N];

void
reset_ints (int *arr)
{
  int i;

  for (i = 0; i < N; i++)
    arr[i] = 0;
}

void
check_result (int *is, int n)
{
  int i;

  for (i = 0; i < N; i++)
    if (is[i] != i * n)
      __builtin_abort ();
}

#define FUNC_CHECK(__a)		\
do				\
  {				\
    reset_ints (ints);		\
    foo##__a (floats, ints);	\
    check_result (ints, __a);	\
  } while (0)


int
main (void)
{
  int i;
  for (i = 0; i < N; i++)
    floats[i] = (float) i;

  FUNC_CHECK (4);
  FUNC_CHECK (8);
  FUNC_CHECK (16);

  return 0;
}

/* { dg-final { scan-assembler-not "fmul\tv\[0-9\]*.*" } } */
/* { dg-final { scan-assembler-times "fcvtzs\tv\[0-9\].4s, v\[0-9\].4s*.*#2" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tv\[0-9\].4s, v\[0-9\].4s*.*#3" 1 } } */
/* { dg-final { scan-assembler-times "fcvtzs\tv\[0-9\].4s, v\[0-9\].4s*.*#4" 1 } } */
