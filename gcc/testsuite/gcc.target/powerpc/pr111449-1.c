/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

/* Ensure vector mode is used for 16-byte by pieces equality compare.  */

int compare1 (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 16) == 0;
}

int compare2 (const char* s1)
{
  return __builtin_memcmp (s1, "0123456789012345", 16) == 0;
}

/* { dg-final { scan-assembler-times {\mvcmpequb\.} 2 } } */
/* { dg-final { scan-assembler-not {\mcmpd\M} } } */
