// { dg-do compile }
// { dg-options "-fgnu-tm" }

__attribute__((transaction_callable))
void func()
{
  __asm__ ("");
}
