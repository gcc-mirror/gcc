// PR c++/102104
// A version of using-variadic1.C where only the qualifying scope
// uses a parameter pack.
// { dg-do compile { target c++17 } }

struct A {
  using target_type = bool*;
};

struct B {
  using target_type = long*;
};

struct C {
  operator bool*();
  operator long*();
};

template<typename Base>
struct cls {
  template<class... Ts>
  struct nested : private Base {
    using Base::operator typename Ts::target_type...;
  };
};

cls<C>::nested<A, B> v1;
bool* a1 = v1;
long* b1 = v1;

cls<C>::nested<B> v2;
bool* a2 = v2; // { dg-error "inaccessible|not an accessible" }
long* b2 = v2;

cls<C>::nested<A> v3;
bool* a3 = v3;
long* b3 = v3; // { dg-error "inaccessible|not an accessible" }
