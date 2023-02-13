// PR c++/102104
// A version of using-variadic1.C where the qualifying scope and the
// terminal name of the using-declaration use different parameter packs.
// { dg-do compile { target c++17 } }

struct A {
  using target_type = bool*;
  operator bool*();
};

struct B {
  using target_type = long*;
  operator long*();
};

template<typename... Bases>
struct cls {
  template<class... Ts>
  struct nested : private Bases... {
    using Bases::operator typename Ts::target_type...;
  };
};

cls<A, B>::nested<A, B> v1;
bool* a1 = v1;
long* b1 = v1;

cls<B>::nested<B> v2;
bool* a2 = v2; // { dg-error "cannot convert" }
long* b2 = v2;

cls<A>::nested<A> v3;
bool* a3 = v3;
long* b3 = v3; // { dg-error "cannot convert" }
