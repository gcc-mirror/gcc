/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX add.n and addi.n instructions.  */

int f (int a, int b)
{
  return a + b;
}

int g (int a)
{
  return a + 32;
}

int h (int a)
{
  return a + 33;
}

/* { dg-final { scan-assembler "\tadd\\.n\t.*" } } */
/* { dg-final { scan-assembler "\taddi\\.n\t.*, 32" } } */
/* { dg-final { scan-assembler "\taddi\t.*, 33" } } */

