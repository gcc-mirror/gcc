// { dg-do assemble  }

namespace N {
  template <class T> struct S;
}

void f()
{
  N::S(); // { dg-error "6:cannot deduce template arguments" "" { target c++17 } } invalid use of template
  // { dg-error "7:missing template arguments" "" { target c++14_down } .-1 }
}
