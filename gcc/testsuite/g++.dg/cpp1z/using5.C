// PR c++/88986
// { dg-do compile { target c++11 } }
// { dg-options "" }

template<typename ...T> struct C : T... {
  using typename T::type ...;  // { dg-warning "pack expansion" "" { target c++14_down } }
  void f() { type value; }  // { dg-error "empty pack" }
};

template struct C<>;
