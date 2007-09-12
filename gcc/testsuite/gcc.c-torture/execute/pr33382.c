struct Foo {
  int i;
  int j[];
};

struct Foo x = { 1, { 2, 0, 2, 3 } };

int foo(void)
{
  x.j[0] = 1;
  return x.j[1];
}

extern void abort(void);

int main()
{
  if (foo() != 0)
    abort();
  return 0;
}
