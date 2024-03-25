/* Test various erroneous or ignored uses of C23 attribute syntax.  */
/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void
f1 (void)
{
  __transaction_atomic [[outer()]] {} /* { dg-error "does not take any arguments" } */
}

void
f2 (void)
{
  __transaction_atomic [[not_a_tm_attribute]] {} /* { dg-warning "attribute directive ignored" } */
}

void
f3 (void)
{
  __transaction_atomic [[unknown_attribute(args of *unknown* attributes need only (be {balanced[(({{[[]]}}))]}!), as per standard C)]] {} /* { dg-warning "attribute directive ignored" } */
}

void
f4 (void)
{
  __transaction_atomic [[gnu::const]] {} /* { dg-warning "attribute directive ignored" } */
}

void
f5 (void)
{
  __transaction_atomic [[bad_namespace::outer]] {} /* { dg-warning "attribute directive ignored" } */
}

void
f6 (void)
{
  __transaction_atomic [[outer, outer]] {} /* { dg-warning "attribute duplicated" } */
}
