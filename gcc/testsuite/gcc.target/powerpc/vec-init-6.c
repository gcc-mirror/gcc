/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

vector int
merge (int a, int b, int c, int d)
{
  return (vector int) { a, b, c, d };
}

/* { dg-final { scan-assembler-times {\mrldi} 2 } } */
/* { dg-final { scan-assembler     "mtvsrd" } } */
/* { dg-final { scan-assembler-not "stw"    } } */
/* { dg-final { scan-assembler-not "lxvw4x" } } */
