/* Test MIPS64 DSP instructions */
/* { dg-do compile } */
/* { dg-options "-mgp64 -mdsp" }  */

/* { dg-final { scan-assembler "\tldx\t" } } */

NOMIPS16 signed long long test (signed long long *a, int index)
{
  return __builtin_mips_ldx (a, index);
}
