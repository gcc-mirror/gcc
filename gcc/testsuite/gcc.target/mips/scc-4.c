/* { dg-do compile } */
/* { dg-options "-mabi=o64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* { dg-final { scan-assembler "\tslt\t" } } */
/* { dg-final { scan-assembler "\tsltu\t\|\txor\t\|\txori\t" } } */

/* This test should work both in mips16 and non-mips16 mode.  */

int
f (long long a, long long b)
{
  return a > 5;
}
