/* Make sure that LEGITIMIZE_ADDRESS is called to handle
   negative displacements.  */

/* { dg-do compile { target s390-*-* } } */
/* { dg-options "-O2" } */

int test (int *addr)
{
  return *(addr - 1);
}

/* { dg-final { scan-assembler "-4096" } } */
/* { dg-final { scan-assembler-not "ahi" } } */

