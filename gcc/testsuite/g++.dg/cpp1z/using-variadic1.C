// PR c++/102104
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
struct cls : private Bases... {
  using Bases::operator typename Bases::target_type...;
};

cls<A, B> v1;
bool* a1 = v1;
long* b1 = v1;

cls<B> v2;
bool* a2 = v2; // { dg-error "cannot convert" }
long* b2 = v2;

cls<A> v3;
bool* a3 = v3;
long* b3 = v3; // { dg-error "cannot convert" }
