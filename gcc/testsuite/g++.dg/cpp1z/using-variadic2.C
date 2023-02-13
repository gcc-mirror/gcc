// PR c++/102104
// A version of using-variadic1a.C where the argument packs have
// different lengths.
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
    using Bases::operator typename Ts::target_type...; // { dg-error "lengths" }
  };
};

cls<A>::nested<A, B> v1; // { dg-message "required from here" }
