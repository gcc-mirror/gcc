/* { dg-do compile } */
/* { dg-require-effective-target supports_stack_clash_protection } */
/* { dg-options "-O3 -fopenmp-simd -march=armv8-a+sve -fstack-clash-protection --param stack-clash-protection-guard-size=16" } */

/* Invoke X (P##n) for n in [0, 7].  */
#define REPEAT8(X, P) \
  X (P##0) X (P##1) X (P##2) X (P##3) X (P##4) X (P##5) X (P##6) X (P##7)

/* Invoke X (n) for all octal n in [0, 39].  */
#define REPEAT40(X) \
  REPEAT8 (X, 0) REPEAT8 (X, 1)  REPEAT8 (X, 2) REPEAT8 (X, 3) REPEAT8 (X, 4)

/* Expect vector work to be done, with spilling of vector registers.  */
void
f2 (int x[40][100], int *y)
{
  /* Try to force some spilling.  */
#define DECLARE(N) int y##N = y[N];
  REPEAT40 (DECLARE);
#pragma omp simd
  for (int i = 0; i < 100; ++i)
    {
#define INC(N) x[N][i] += y##N;
      REPEAT40 (INC);
    }
}

/* SVE spill, requires probing as vector size is unknown at compile time.  */

/* { dg-final { scan-assembler-times {str\s+xzr, \[sp, 0\]} 1 } } */
/* { dg-final { scan-assembler-times {cmp\s+x[0-9]+, 61440} 1 } } */
/* { dg-final { scan-assembler-times {sub\s+x[0-9]+, x[0-9]+, 61440} 1 } } */
