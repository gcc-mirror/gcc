/* Check that trapa / interrput_handler attributes can paired in
   either order.  */
/* { dg-do compile { target "sh*-*-*" } }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler "trapa\[ \t\]\[ \t\]*#4"} }  */
/* { dg-final { scan-assembler-times "trapa" 1 } }  */

void h0 (void) __attribute__ ((trap_exit (4))) __attribute__ ((interrupt_handler));
void h1 (void) __attribute__ ((interrupt_handler)) __attribute__ ((trap_exit (5)));

void
foo (void)
{
}

void
h0 (void)
{
}

void delay
(int a)
{
}

int
main (void)
{
  return 0;
}
