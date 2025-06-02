// PR c++/120414
// { dg-additional-options "-fmodules" }
// { dg-module-cmi m }

export module m;

template <int n>
struct Base {
  static constexpr int base_static_mbr_n = n;
};

template <int n>
struct Derived : Base<n> {
  using Base<n>::base_static_mbr_n;
  static constexpr int go(int x = base_static_mbr_n) { return x; }
};

template struct Derived<1>;
