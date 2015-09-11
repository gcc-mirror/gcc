/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX callr.n, jmpr.n, ret.n instructions.  */

typedef int (*F) (void);

int x (F f)
{
  f ();

  /* Note that the compiler might generate a return via pop.n or ldwm;
     the test below is to make sure that it doesn't generate a 32-bit
     return instruction.  */
  return 3;
}

int y (F f)
{
  return f ();
}

/* { dg-final { scan-assembler "\tcallr\\.n\t.*" } } */
/* { dg-final { scan-assembler-not "\tret$" } } */
/* { dg-final { scan-assembler "\tjmpr\\.n\t.*" } } */
