/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX mov.n and movi.n instructions.  */

extern void f (int a, int b, int c, int d);

int g (int x, int y, int z)
{
  f (100, x, y, z);
  return -1;
}

/* We should always get mov.n and never mov when compiling with -mcdx.  */
/* { dg-final { scan-assembler "\tmov\\.n\t.*" } } */
/* { dg-final { scan-assembler-not "\tmov\t.*" } } */

/* Both of the constant loads are expressible with movi.n.  */
/* { dg-final { scan-assembler "\tmovi\\.n\t.*, 100" } } */
/* { dg-final { scan-assembler "\tmovi\\.n\t.*, -1" } } */
