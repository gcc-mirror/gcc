/* Check that we can use the swm/lwm instructions.  */
/* { dg-options "-mabi=32 (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

int bar (int, int, int, int, int);

MICROMIPS int
foo (int n, int a, int b, int c, int d)
{
  int i, j;

  i = bar (n, a, b, c, d);
  j = bar (n, a, b, c, d);
  return i + j;
}

/* { dg-final { scan-assembler "\tswm\t\\\$16-\\\$2(0|1),\\\$31" } } */
/* { dg-final { scan-assembler "\tlwm\t\\\$16-\\\$2(0|1),\\\$31" } } */
