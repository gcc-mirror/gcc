/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect -mprivileged" } */
/* { dg-require-effective-target rop_ok } */

/* Verify that privileged ROP-protect instructions are inserted when a
   call is present.  */

extern void foo (void);

int bar ()
{
  foo ();
  return 5;
}

/* { dg-final { scan-assembler-times {\mhashstp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mhashchkp\M} 1 } } */
