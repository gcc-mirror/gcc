
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

static void __attribute__((noinline))
called_by_test_2 (int **out)
{
  int i = 42;
  *out = &i;  
}

int test_2 (void)
{
  int *ptr;
  called_by_test_2 (&ptr);
  return *ptr; /* { dg-warning "dereferencing pointer 'ptr' to within stale stack frame" } */
}

static int __attribute__((noinline))
called_by_test_3 (int **out)
{
  int i = 42;
  *out = &i;
  return i;
}

int test_3 (void)
{
  int *lhs_ptr;
  *lhs_ptr = called_by_test_3 (&lhs_ptr); /* { dg-warning "use of uninitialized value 'lhs_ptr'" } */
  return *lhs_ptr;
}
