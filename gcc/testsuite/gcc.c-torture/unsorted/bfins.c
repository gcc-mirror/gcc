struct foo
{
  unsigned j : 16;
  unsigned i : 16;
};

struct foo
foo (a, b)
     struct foo a;
     int b;
{
  a.j = 123;
  a.i = b;
  return a;
}
