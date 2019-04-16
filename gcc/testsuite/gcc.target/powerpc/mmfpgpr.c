/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power6x -mmfpgpr" } */
/* { dg-final { scan-assembler "mffgpr" } } */
/* { dg-final { scan-assembler "mftgpr" } } */

/* Test that we generate the instructions to move between the GPR and FPR
   registers under power6x.  */

extern long return_long (void);
extern double return_double (void);

double return_double2 (void)
{
  return (double) return_long ();
}

long return_long2 (void)
{
  return (long) return_double ();
}
