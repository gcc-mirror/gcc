// PR middle-end/94303
// { dg-do run }

struct A {
  int d = 9;
  A () = default;
  A (int x) : d(x) {}
  void foo () { if (d < 1) __builtin_abort (); }
};

A a[3] = { 1 };

int
main ()
{
  a[2].foo ();
}
