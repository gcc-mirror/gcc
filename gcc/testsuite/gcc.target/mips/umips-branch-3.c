/* { dg-options "(-mmicromips) -mcompact-branches=optimal" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void MICROMIPS
foo (void)
{
  return;
}

/* { dg-final { scan-assembler "\tjrc\t\\\$31\n" } } */
