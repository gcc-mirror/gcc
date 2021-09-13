// { dg-do assemble  }

namespace N {
  template <class T> struct S;
}

void f()
{
  N::S(); // { dg-error "8:class template argument deduction failed|no match" "" { target c++17 } } invalid use of template
  // { dg-error "7:missing template arguments" "" { target c++14_down } .-1 }
}
