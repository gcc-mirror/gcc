// P1094R2
// { dg-do compile { target c++11 } }
// { dg-options "-Wpedantic" }

namespace A::inline B::C { // { dg-warning "nested inline namespace definitions only" "" { target c++17_down } }
// { dg-warning "nested namespace definitions only available" "" { target c++14_down } .-1 }
  int i;
}

namespace D::E::inline F { // { dg-warning "nested inline namespace definitions only" "" { target c++17_down } }
// { dg-warning "nested namespace definitions only available" "" { target c++14_down } .-1 }
  int j;
}

inline namespace X {
  int x;
}

// Make sure the namespaces are marked inline.
void
g ()
{
  A::B::C::i++;
  A::C::i++;
  D::E::j++;
  D::E::F::j++;
  X::x++;
  x++;
}
