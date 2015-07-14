/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX sub.n, subi.n, and neg.n instructions.  */

int f (int a, int b)
{
  return a - b;
}

int g (int a)
{
  return a - 32;
}

int h (int a)
{
  return -a;
}

/* { dg-final { scan-assembler "\tsub\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tsubi\\.n\t.*, 32" } } */
/* { dg-final { scan-assembler "\tneg\\.n\t.*" } } */
