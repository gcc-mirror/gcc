/* { dg-do compile { target be } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-skip-if "" { has_arch_ppc64 && ilp32 } } */
/* { dg-final { scan-assembler-not {\mb[l]? memcmp\M} } } */

/* Test that it does expand for memcmpsi instead of calling library on
   P7 BE when length is less than 32 bytes.  */

int foo (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 31);
}
