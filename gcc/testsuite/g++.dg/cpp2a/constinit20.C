// PR c++/119652
// { dg-do compile { target c++20 } }

struct __shared_count {
  constexpr __shared_count() {}
  ~__shared_count();
  int _M_pi = 0;
};
struct shared_ptr {
  __shared_count _M_refcount;
};
struct A {
  A() = default;
  shared_ptr m;
};
constinit A a;
constinit A b {};
constinit A c = {};
