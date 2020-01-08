// PR c++/11116

template <typename T> struct S {};

void f() {
  throw S (); // { dg-error "9:cannot deduce template arguments" "" { target c++17 } }
  // { dg-error "11:missing template arguments" "" { target c++14_down } .-1 }
}
