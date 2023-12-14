static void __attribute__((noinline))
__analyzer_consume_n_uints (int num, ...)
{
  __builtin_va_list ap;
  __builtin_va_start (ap, num);

  int i, v;
  for (i = 0; i < num; i++)
    v = __builtin_va_arg (ap, unsigned int);

  __builtin_va_end (ap);
}

void test_uint (unsigned int x)
{
  __analyzer_consume_n_uints (1, x);
}

void test_3_uints (unsigned int x, unsigned int y, unsigned int z)
{
  __analyzer_consume_n_uints (3, x, y, z);
}
