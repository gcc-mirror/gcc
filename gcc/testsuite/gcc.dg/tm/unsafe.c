/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

__attribute__((transaction_callable))
static int func ()
{
  return 12345;
}

int main()
{
  __transaction_atomic { return func(); } /* { dg-error "unsafe function call .func. " } */
}
