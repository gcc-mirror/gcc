// PR c++/92594
// { dg-do compile { target c++11 } }

template <typename _Head> struct tuple {
  tuple() : _M_head_impl() {}
  _Head _M_head_impl;
};
template <typename type0> struct pod_tuple { type0 _head; };
struct e {};
struct bar : e {
  using e::e;
};
int main() { tuple<pod_tuple<bar>> a; }
