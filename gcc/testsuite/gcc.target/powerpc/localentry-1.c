/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -mpcrel" } */
/* { dg-require-effective-target powerpc_elfv2 } */
/* { dg-require-effective-target power10_ok } */

/* Ensure we generate ".localentry fn,1" for both leaf and non-leaf functions.
   At present, -mcpu=power10 does not enable pc-relative mode, so make sure we
   enable it to be able to check for .localentry.  */

extern int y (int);

int x (void)
{
  return y (5);
}

void z (void) { };

/* { dg-final { scan-assembler {\.localentry\t\mx,1\M} } } */
/* { dg-final { scan-assembler {\.localentry\t\mz,1\M} } } */
