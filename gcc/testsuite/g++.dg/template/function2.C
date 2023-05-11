// PR c++/83258

template<void(*)()> struct A { };

int main() {
  struct B { static void f() { } };
  A<B::f> a; // { dg-error "linkage" "" { target c++14_down } }
}
