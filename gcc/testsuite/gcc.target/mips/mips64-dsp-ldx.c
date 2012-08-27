/* Test MIPS64 DSP instructions */
/* { dg-do compile } */
/* { dg-options "-mgp64 -mdsp" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* { dg-final { scan-assembler "\tldx\t" } } */

NOMIPS16 signed long long test (signed long long *a, int index)
{
  return a[index];
}
