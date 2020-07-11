// PR rtl-optimization/94618
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fnon-call-exceptions -fcompare-debug" }

struct S
{
  int a, b, c;
  int foo () noexcept { return a; }
  int bar () noexcept { return b; }
  void baz (int);
  void qux () { if (c) for (int x = foo (); x != bar (); ) baz (x); }
};

struct T
{
  S s;
  void foo ();
};

void
T::foo ()
{
  s.qux ();
  s.qux ();
}
