/* { dg-do compile } */

__builtin_va_list *pap;

void
fn1 (void)
{
  __builtin_va_arg (pap, double); /* { dg-error "first argument to 'va_arg' not of type 'va_list'" } */
}
