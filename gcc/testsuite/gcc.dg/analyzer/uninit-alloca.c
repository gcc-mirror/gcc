/* { dg-require-effective-target alloca } */

int test_1 (void)
{
  int *p = __builtin_alloca (sizeof (int)); /* { dg-message "region created on stack here" } */
  return *p; /* { dg-warning "use of uninitialized value '\\*p'" } */
}
