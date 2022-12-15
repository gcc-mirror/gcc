// PR c++/102104
// A version of of using-variadic1.C where only the terminal name
// uses a parameter pack.
// { dg-do compile { target c++17 } }

struct A {
  operator bool*();
};

struct B {
  operator bool*();
};

struct C {
  using target_type = bool*;
};

template<typename... Bases>
struct cls {
  template<class T>
  struct nested : private Bases... {
    using Bases::operator typename T::target_type...;
  };
};

cls<A, B>::nested<C> v1;
bool* a1 = v1; // { dg-error "ambiguous" }

cls<A>::nested<C> v2;
bool* a2 = v2;

cls<B>::nested<C> v3;
bool* a3 = v3;
