// PR c++/118923
// { dg-do run { target c++14 } }

struct A {
  int a[3] = { 0, 0, 0 };
  int *begin () { return a; }
  int *end () { return a + 3; }
};

struct B {
  A foo () { return { 1, 2, 3 }; }
  A bar () { return { 1, 2, 3 }; }
  bool baz () { return b; }
  bool b = false;
  static B c;
};

B B::c;

inline B *
qux ()
{
  return &B::c;
}

void 
foo ()
{
  auto d = qux ()->baz () ? &B::foo : &B::bar;
  for (const int &r : (qux ()->*d) ())
    ;
}

int
main ()
{
  foo ();
}
