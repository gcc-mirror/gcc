// PR c++/102163
// { dg-do compile { target c++11 } }

struct O {
  constexpr O(int) { }
};

union _Variadic_union {
  constexpr _Variadic_union(int __arg) : _M_rest(__arg) { }
  int _M_first;
  O _M_rest;
};

constexpr _Variadic_union u(42);

struct _Variant_storage {
  constexpr _Variant_storage() : _M_u(42) {}
  _Variadic_union _M_u;
};

constexpr _Variant_storage w;
