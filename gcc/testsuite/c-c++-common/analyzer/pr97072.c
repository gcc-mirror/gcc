void unknown_fn_1 (void *) __attribute__((nothrow));

void test_1 (int co, int y)
{
  void *p = __builtin_malloc (1024);
  void **q;
  unknown_fn_1 (&q);
  *q = p;
}
