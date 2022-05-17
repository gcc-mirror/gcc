/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#define NULL ((void *)0)

void test_sentinel (int arg, ...)
{
  const char *s;
  __builtin_va_list ap;
  __builtin_va_start (ap, arg);
  while (s = __builtin_va_arg (ap, char *)) /* { dg-warning "'ap' has no more arguments \\(2 consumed\\)" } */
    {
      (void)s;
    }
  __builtin_va_end (ap);
}

void test_caller (void)
{
  test_sentinel (42, "foo", "bar", NULL);
}

void missing_sentinel (void)
{
  test_sentinel (42, "foo", "bar");
}
