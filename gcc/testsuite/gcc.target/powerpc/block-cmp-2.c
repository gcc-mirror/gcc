/* { dg-do compile } */
/* { dg-require-effective-target opt_mstrict_align } */
/* { dg-options "-O2 -mstrict-align" } */
/* { dg-final { scan-assembler-times {\mb[l]? memcmp\M} 1 } }  */

/* Test that it calls library for block memory compare when strict-align
   is set.  The flag causes rs6000_slow_unaligned_access returns true.  */

int foo (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 20);
}
