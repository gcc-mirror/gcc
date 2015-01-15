/* Test MIPS32R6 LSA instruction */
/* { dg-do compile } */
/* { dg-options "-mgp32 (HAS_LSA)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* { dg-final { scan-assembler "\tlsa\t" } } */

NOMIPS16 signed short test (signed short *a, int index)
{
  return a[index];
}
