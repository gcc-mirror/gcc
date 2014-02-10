/* { dg-options "(-mmicromips) addressing=absolute" } */

void foo (void);

int MICROMIPS
a (void)
{
  foo ();
  return 0;
}

/* { dg-final { scan-assembler "\tjals\tfoo\n\tnop" } } */
