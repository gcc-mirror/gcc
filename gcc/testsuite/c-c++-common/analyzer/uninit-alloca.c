int test_1 (void)
{
  int *p = (int *) __builtin_alloca (sizeof (int)); /* { dg-message "region created on stack here" } */
  return *p; /* { dg-warning "use of uninitialized value '\\*p'" } */
}
