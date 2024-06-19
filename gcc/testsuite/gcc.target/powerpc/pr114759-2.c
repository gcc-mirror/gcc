/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mrop-protect" } */
/* { dg-require-effective-target rop_ok } Only enable on supported ABIs.  */

/* Verify we generate ROP-protect hash insns when compiling for Power8.  */

extern void foo (void);

int
bar (void)
{
  foo ();
  return 5;
}

/* { dg-final { scan-assembler-times {\mhashst\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhashchk\M} 1 } } */
