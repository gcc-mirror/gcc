/* Check whether rte is generated only for an ISR.  */
/* { dg-do compile { target h8300-*-* } }  */
/* { dg-options "-O" }  */
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
/* Check whether rte is generated only for an ISR.  */
/* { dg-do compile { target h8300-*-* } }  */
/* { dg-options "-O" }  */
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
