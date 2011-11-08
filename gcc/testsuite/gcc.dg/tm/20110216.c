/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

int george;

__attribute__((transaction_callable))
void q1()
{
  __transaction_atomic {
      george=999;
  }
  q1();
}

/* { dg-final { scan-assembler-not "_ITM_getTMCloneOrIrrevocable" } } */
