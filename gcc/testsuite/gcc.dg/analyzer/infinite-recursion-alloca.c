typedef __SIZE_TYPE__ size_t;

int test_alloca_1 (void)
{
  void *buf = __builtin_alloca (1024);
  return test_alloca_1 (); /* { dg-warning "-Wanalyzer-infinite-recursion" } */
}

int test_alloca_2 (size_t n)
{
  void *buf = __builtin_alloca (n);
  return test_alloca_2 (n); /* { dg-warning "-Wanalyzer-infinite-recursion" } */
}

int test_alloca_3 (size_t n)
{
  void *buf = __builtin_alloca (n);
  return test_alloca_2 (n - 1);
}

int test_alloca_4 (size_t n)
{
  void *buf = __builtin_alloca (n);
  if (n > 0)
    return test_alloca_2 (n - 1);
  return 42;
}
