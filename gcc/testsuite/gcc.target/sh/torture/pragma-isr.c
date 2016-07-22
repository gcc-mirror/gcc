/* Check whether rte is generated for two ISRs.  */
/* { dg-do compile }  */
/* { dg-final { scan-assembler-times "rte" 2 } }  */

extern void foo (void);

#pragma interrupt
void
isr1 (void)
{
  foo ();
}

#pragma interrupt
void
isr2 (void)
{
  foo ();
}
