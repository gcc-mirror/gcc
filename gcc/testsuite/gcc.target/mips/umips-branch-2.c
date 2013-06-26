/* { dg-options "(-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

int MICROMIPS
foo (int *x)
{
  return x[5000];
}

/* { dg-final { scan-assembler "\tjr?\t\\\$31\n\tlw\t\\\$2,20000\\(\\\$4\\)" } } */
