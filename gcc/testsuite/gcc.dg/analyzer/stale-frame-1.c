
int *global_ptr;

static void __attribute__((noinline))
called_by_test_1 (void)
{
  int i = 42;
  global_ptr = &i;  
}

int test_1 (void)
{
  called_by_test_1 ();
  return *global_ptr; /* { dg-warning "dereferencing pointer 'global_ptr' to within stale stack frame" } */
}
