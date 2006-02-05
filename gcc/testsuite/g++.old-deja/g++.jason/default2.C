// { dg-do assemble  }
// PRMS Id: 5921
// Bug: default arguments containing constructor calls persist incorrectly.

class foo
{
 public:
  foo();
  foo(int x);
 public:
  int iamamember;
};

class bar
{
 public:
  bar();
  int memberfunction(int i, const char *j, double k, foo foo1 = foo(0));
};

int
pain(bar *bar1)
{
  return bar1->memberfunction(1, "x", 0.0);
}

int
pain2(bar *bar1)
{
  return bar1->memberfunction(1, "x", 0.0);
}
