// { dg-do run  }
// Test for proper handling of temporaries in ?: exprs.

extern "C" int printf (const char *, ...);
int c = 0, d = 0;

class A {
public:
  A() { ++c; }
  A(const A&) { ++c; }
  ~A() { ++d; }
};

A f (const A& a)
{
  return (c ? A() : A());
}

int main()
{
  {
    f (c ? A() : A());
  }
  printf ("%d %d\n", c, d);
  return c != d || c != 2;
}
