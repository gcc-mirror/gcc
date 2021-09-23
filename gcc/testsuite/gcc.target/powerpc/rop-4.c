/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect" } */
/* { dg-require-effective-target powerpc_elfv2 } */

/* Verify that no ROP-protect instructions are inserted when no
   call is present.  */


int bar ()
{
  return 5;
}

/* { dg-final { scan-assembler-not {\mhashst\M} } } */
/* { dg-final { scan-assembler-not {\mhashchk\M} } } */
