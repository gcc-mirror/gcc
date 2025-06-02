/* Check whether the second -march overrides the first.  */
/* { dg-do compile { target rv64 } } */
/* { dg-options "-O3 -march=rv64gc -march=sifive-p670" } */

void
foo (char *a, char *b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i] + 1;
}

/* { dg-final { scan-assembler "vset" } } */
/* { dg-final { scan-assembler "zvl128b" } } */
