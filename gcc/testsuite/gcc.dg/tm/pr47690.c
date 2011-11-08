/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

int george;

void q1()
{
  __transaction_atomic {
      george=999;
  }
  q1();
}

/* { dg-final { scan-assembler-not "ZGTt2q1" } } */
