
typedef  void (*p1)(void);
typedef  void (*proc)(p1);

void myfunc (p1 p)
{
}

void test (void) {}

main()
{
  proc foo;

  foo = myfunc;
  foo(test);
}
