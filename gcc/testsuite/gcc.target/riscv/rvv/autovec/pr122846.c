/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=conv-dynamic" } */

int
foo (const char *x, const char *y)
{
  int sum = 0;
  for (int i = 0; i < 1024; i++)
    sum += x[i] * y[i];
  return sum;
}

/* One for the initial value, one for the reduction.  */
/* { dg-final { scan-assembler-times ",m4," 2 } } */
