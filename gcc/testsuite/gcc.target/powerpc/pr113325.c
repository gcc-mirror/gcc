/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-not {\mxxpermdi\M} } } */

void* foo (void* s1)
{
  return __builtin_memset (s1, 0, 32);
}
