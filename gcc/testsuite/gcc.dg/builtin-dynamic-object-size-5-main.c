#ifdef N
typedef __SIZE_TYPE__ size_t;

char buf[N];

void test1 (size_t);
void test2 (size_t);
void test3 (size_t);
void test4 (size_t);
void test5 (size_t);
void test6 (size_t);
void test7 (size_t);

int
main (void)
{
  test1 (42);
  test2 (42);
  test3 (42);
  test4 (42);
  test5 (42);
  test6 (42);
  test7 (42);
  return 0;
}
#else
int
main (void)
{
  return 0;
}
#endif
