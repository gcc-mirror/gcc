/* { dg-additional-options "-ftrivial-auto-var-init=uninitialized" } */

int test_1 (void)
{
  int i; /* { dg-message "region created on stack here" } */
  return i; /* { dg-warning "use of uninitialized value 'i'" } */
}
