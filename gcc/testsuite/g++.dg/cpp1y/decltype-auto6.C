// PR c++/101165 - P2266R1 - Simpler implicit move
// { dg-do compile { target c++14 } }
// A variant of cxx23/elision1.C:eight, just with ().

struct Widget {
  Widget(Widget&&);
};

Widget val();

decltype(auto)
foo ()
{
  decltype(auto) x = val();  // OK, x is Widget
  // We deduce the return type to int&&, therefore we're doing something
  // we ought not to be doing -- returning a reference to a local variable!
  // In C++20, we deduce to int&, but that has the same problem!
  return (x); // { dg-warning "reference to local variable" }
}
