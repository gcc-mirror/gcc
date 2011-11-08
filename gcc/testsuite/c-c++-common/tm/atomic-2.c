/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

int g;
void f(void)
{
  __transaction_atomic {
    g++;
  }
}
