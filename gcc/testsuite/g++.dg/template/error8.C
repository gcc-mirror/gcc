// PR c++/11116

template <typename T> struct S {};

void f() {
  throw S (); // { dg-error "template" }
}
