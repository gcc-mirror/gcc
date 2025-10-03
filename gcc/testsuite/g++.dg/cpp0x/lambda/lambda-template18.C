// PR c++/122127
// { dg-do compile { target c++11 } }

template <int> struct resize;
template <int _Np> using resize_t = resize<_Np>;
template <class _Ap> struct basic_mask {
  void _M_chunk() {
    constexpr int __rem = 1;
    [&] { resize_t<__rem>(); }();
  }
};
