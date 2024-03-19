/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=7450 -fno-math-errno" } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "" { has_arch_ppc64 } } */
/* { dg-final { scan-assembler-times {\mfctiw\M} 2 } }  */
/* { dg-final { scan-assembler-times {\mstfiwx\M} 2 } }  */

int test1 (double a)
{
  return __builtin_irint (a);
}

int test2 (float a)
{
  return __builtin_irint (a);
}
