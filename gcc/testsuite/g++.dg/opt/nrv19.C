// PR c++/84978
// { dg-do compile }

struct S {
  void (*fn)();
  int a[10];
};

S
foo ()
{
  S s;
  s.fn ();
  return s;
}
