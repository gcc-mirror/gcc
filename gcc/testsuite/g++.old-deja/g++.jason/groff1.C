// PRMS Id: 3744
// Bug: unswitching a COND_EXPR initializer fails to set SIDE_EFFECTS on the
// result, so expand_expr ignores it.

extern "C" {
  int printf(const char *,...);
  void exit(int);
}

struct A {
  int x;
  int y;

  A() : x(0), y(0) { }
};

struct S {
  S() : flags(0) { }
  unsigned flags;
  A from;
  void foo(const A &pos);
};

void S::foo(const A &pos)
{
  A a = flags ? from : pos;
  printf("%d %d\n", a.x, a.y);
  if (a.x != 17 || a.y != 12)
    exit (1);
}

int main()
{
  A pos;
  pos.x = 17;
  pos.y = 12;
  S s;
  s.foo(pos);
  return 0;
}
