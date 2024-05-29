/* { dg-do compile { target { powerpc*-*-linux* && lp64 } } } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Check code generation for direct move for long types.  */

void
test (vector double *p)
{
  vector double v1 = *p;
  vector double v2;
  vector double v3;
  vector double v4;

  /* Force memory -> FPR load.  */
  __asm__ (" # reg %x0" : "+d" (v1));

  /* force VSX -> GPR direct move.  */
  v2 = v1;
  __asm__ (" # reg %0" : "+r" (v2));

  /* Force GPR -> Altivec direct move.  */
  v3 = v2;
  __asm__ (" # reg %x0" : "+v" (v3));
  *p = v3;
}

/* { dg-final { scan-assembler "mfvsrd"  } } */
/* { dg-final { scan-assembler "mfvsrld" } } */
/* { dg-final { scan-assembler "mtvsrdd" } } */


