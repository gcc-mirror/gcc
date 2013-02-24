/* Check that we can generate the MOVEP instruction.  */
/* { dg-options "-fpeephole2 -mgp32 (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

int bar (int, int);

int MICROMIPS
foo (int n, int a)
{
  return bar (0, 0);
}

/* { dg-final { scan-assembler "\tmovep\t\\\$4,\\\$5,\\\$0,\\\$0" } } */
