// PR c++/88986
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct B1 { typedef int type; };
struct B2 { typedef double type; };

template<typename ...T> struct C : T... {
  using typename T::type ...;  // { dg-warning "pack expansion" "" { target c++14_down } }
  // { dg-error "redeclaration" "" { target *-*-* } .-1 }
  void f() { type value; }
};

template struct C<B1, B2>;
