/* Check whether trapa is generated only for an ISR.  */
/* { dg-do compile }  */
/* { dg-options "-O" }  */
/* { dg-final { scan-assembler-times "trapa\[ \t\]\[ \t\]*#4" 1 } }  */

#pragma interrupt
void isr (void) __attribute__ ((trap_exit (4)));

void
isr (void)
{
}

void
delay (int a)
{
}

int
main (void)
{
  return 0;
}
