// { dg-do compile }

const int* bar();

const int&
foo1()
{
  static int empty;
  const int* x = bar();
  return (x ? *x : empty);      // { dg-bogus ".*" "" { xfail *-*-* } }
}

const int&
foo2()
{
  static int empty;
  const int* x = bar();
  const int& r = (x ? *x : empty);
  return (r);
}

