/* Check that we can generate the MOVEP instruction.  */
/* { dg-options "-mgp32 -fpeephole2 (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

long long bar (long long, long long);

MICROMIPS long long
foo (long long n, long long a)
{
  long long i, j;

  i = bar (n, a);
  j = bar (n, a);
  return i + j;
}
/* { dg-final { scan-assembler "\tmovep\t" } } */
