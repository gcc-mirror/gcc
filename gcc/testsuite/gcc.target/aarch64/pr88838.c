/* { dg-do compile } */
/* { dg-options "-S -O3 -march=armv8.2-a+sve" } */

void
f (int *restrict x, int *restrict y, int *restrict z, int n)
{
    for (int i = 0; i < n; i += 1)
          x[i] = y[i] + z[i];
}

/* { dg-final { scan-assembler-not "sxtw" } } */
