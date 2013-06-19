/* Check whether rte is generated only for an ISRs.  */
/* { dg-do compile { target "sh*-*-*" } }  */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-times "rte" 1 } }  */

#pragma interrupt
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
