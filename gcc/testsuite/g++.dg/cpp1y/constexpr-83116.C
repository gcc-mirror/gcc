// PR c++/83116
// { dg-do run { target c++14 } }
// { dg-options "-O2" }

struct S {
  constexpr S () : s(0) { foo (); }
  constexpr int foo () { return s; }
  int s;
};

int
main ()
{
  static S var;
  var.s = 5;
  if (var.s != 5 || var.foo () != 5)
    __builtin_abort ();
}
