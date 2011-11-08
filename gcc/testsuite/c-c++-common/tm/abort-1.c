/* { dg-do compile } */

void f(void)
{
  __transaction_cancel;		/* { dg-error "without transactional" } */
}
