/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mno-vsx" } */
/* { dg-skip-if "" { has_arch_ppc64 && ilp32 } } */
/* { dg-final { scan-assembler-not {\mb[l]? memcmp\M} } }  */

/* Test that it still can do expand for memcmpsi instead of calling library
   on P8 with vsx disabled.  */

int foo (const char* s1, const char* s2)
{
  return __builtin_memcmp (s1, s2, 20);
}
