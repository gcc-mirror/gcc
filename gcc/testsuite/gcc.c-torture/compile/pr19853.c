struct test { int *x; } global_test;
int global_int;

int flag;

void test (char *dummy)
{
  static const struct test const_test = { &global_int };
  struct test local_test;

  int i;
  for (i = 0; i < 1; i++)
    *dummy = 0;
  if (flag)
    __builtin_memset (dummy, 0, 16);

  local_test = const_test;
  global_test = local_test;
}
