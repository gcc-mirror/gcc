// PR c++/78931
// { dg-do run { target c++11 } }
// { dg-options "" }

int
main ()
{
  int x = 99;
  struct S { int &x; };
  S s{x};
  auto [p] = s;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  return p - 99;
}
