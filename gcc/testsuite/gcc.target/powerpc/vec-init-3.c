/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

vector long
merge (long a, long b)
{
  return (vector long) { a, b };
}

/* { dg-final { scan-assembler "mtvsrdd" } } */
