/* { dg-options "(-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

int MICROMIPS
foo (void)
{
  return 0;
}

/* { dg-final { scan-assembler "\tjr?\t\\\$31\n\tmove\t\\\$2,\\\$0" } } */
