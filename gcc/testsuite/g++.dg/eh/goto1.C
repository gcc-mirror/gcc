extern "C" void abort ();

static int count;

struct S {
  S() { ++count; }
  ~S() { --count; }
};

int foo(int p)
{
  S s1;
  {
    S s2;
    if (p)
      goto L;
    else
      return 1;
  }
  foo (p);
 L:
  return 0;
}

int main()
{
  foo(0);
  if (count != 0)
    abort ();
  foo(1);
  if (count != 0)
    abort ();
  return 0;
}
