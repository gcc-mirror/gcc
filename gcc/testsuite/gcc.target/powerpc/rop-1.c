/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect" } */
/* { dg-require-effective-target rop_ok } */

/* Verify that ROP-protect instructions are inserted when a
   call is present.  */

extern void foo (void);

int bar ()
{
  foo ();
  return 5;
}

/* { dg-final { scan-assembler-times {\mhashst\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhashchk\M} 1 } } */
