/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void foo(void (*fn)(void))
{
  __transaction_relaxed {
    fn();
  }
}
