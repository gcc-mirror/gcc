/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */
/* { dg-require-effective-target powerpc_elfv2 } */

/* Ensure we generate ".localentry fn,1" for both leaf and non-leaf
   functions.  */

extern int y (int);

int x (void)
{
  return y (5);
}

void z (void) { };

/* { dg-final { scan-assembler {\.localentry\t\mx,1\M} } } */
/* { dg-final { scan-assembler {\.localentry\t\mz,1\M} } } */
