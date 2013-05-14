/* Check that we can use the swm instruction to save $16, $17 and $31.  */
/* { dg-options "-mgp32 (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void bar (void);

MICROMIPS void
foo (void)
{
  bar ();
  asm volatile ("" ::: "$16", "$17");
}
/* { dg-final { scan-assembler "\tswm\t\\\$16-\\\$17,\\\$31" } } */
/* { dg-final { scan-assembler "\tlwm\t\\\$16-\\\$17,\\\$31" } } */
