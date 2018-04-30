/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -fopenmp-simd -fno-omit-frame-pointer" } */

/* Invoke X (P##n) for n in [0, 7].  */
#define REPEAT8(X, P) \
  X (P##0) X (P##1) X (P##2) X (P##3) X (P##4) X (P##5) X (P##6) X (P##7)

/* Invoke X (n) for all octal n in [0, 39].  */
#define REPEAT40(X) \
  REPEAT8 (X, 0) REPEAT8 (X, 1)  REPEAT8 (X, 2) REPEAT8 (X, 3) REPEAT8 (X, 4)

volatile int testi, sink;

/* Take 2 stack arguments and throw to f3.  */
void __attribute__ ((weak))
f1 (int x[40][100], int *y, int z1, int z2, int z3, int z4,
    int z5, int z6, int z7, int z8)
{
  /* A wild write to x and y.  */
  sink = z1;
  sink = z2;
  sink = z3;
  sink = z4;
  sink = z5;
  sink = z6;
  sink = z7;
  sink = z8;
  asm volatile ("" ::: "memory");
  if (y[testi] == x[testi][testi])
    throw 100;
}

/* Expect vector work to be done, with spilling of vector registers.  */
void __attribute__ ((weak))
f2 (int x[40][100], int *y)
{
  /* Try to force some spilling.  */
#define DECLARE(N) int y##N = y[N];
  REPEAT40 (DECLARE);
  for (int j = 0; j < 20; ++j)
    {
      f1 (x, y, 1, 2, 3, 4, 5, 6, 7, 8);
#pragma omp simd
      for (int i = 0; i < 100; ++i)
	{
#define INC(N) x[N][i] += y##N;
	  REPEAT40 (INC);
	}
    }
}

/* Catch an exception thrown from f1, via f2.  */
void __attribute__ ((weak))
f3 (int x[40][100], int *y, int *z)
{
  volatile int extra = 111;
  try
    {
      f2 (x, y);
    }
  catch (int val)
    {
      *z = val + extra;
    }
}

static int x[40][100];
static int y[40];
static int z;

int
main (void)
{
  f3 (x, y, &z);
  if (z != 211)
    __builtin_abort ();
  return 0;
}
