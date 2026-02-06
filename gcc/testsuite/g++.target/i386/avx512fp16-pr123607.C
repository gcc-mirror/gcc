// PR target/123607
// { dg-do assemble { target avx512fp16 } }
// { dg-options "-std=c++23 -O2 -mavx512fp16" }

namespace std::simd {
template <int> struct basic_mask {
  bool _M_data;
  bool operator[](int) { return _M_data; }
};
template <int _Ap>
  requires(_Ap > 1)
struct basic_mask<_Ap> {
  static constexpr int _N0 = _Ap / 2;
  using _Mask1 = basic_mask<_N0>;
  _Mask1 _M_data1;
  static basic_mask _S_init(basic_mask<_N0>, _Mask1 __y) {
    basic_mask __r;
    __r._M_data1 = __y;
    return __r;
  }
};
template <int _Ap> struct basic_vec {
  _Float16 _M_data;
  using mask_type = basic_mask<_Ap>;
  friend mask_type operator<(basic_vec __x, basic_vec __y) {
    return mask_type(__x._M_data < __y._M_data);
  }
  friend basic_vec __select_impl(mask_type __k, basic_vec __t, basic_vec __f) {
    return __k[0] ? __t : __f;
  }
};
template <int _Ap>
  requires(_Ap > 1)
struct basic_vec<_Ap> {
  static constexpr int _N0 = _Ap / 2;
  using _DataType1 = basic_vec<_N0>;
  _DataType1 _M_data0;
  _DataType1 _M_data1;
  using mask_type = basic_mask<_Ap>;
  static basic_vec _S_init(_DataType1 __y) {
    basic_vec __r;
    __r._M_data1 = __y;
    return __r;
  }
  friend mask_type operator<(basic_vec __x, basic_vec __y) {
    return mask_type::_S_init(__x._M_data0 < __y._M_data0,
                              __x._M_data1 < __y._M_data1);
  }
  friend basic_vec __select_impl(mask_type __k, basic_vec __t, basic_vec __f) {
    return _S_init(__select_impl(__k._M_data1, __t._M_data1, __f._M_data1));
  }
};
basic_vec<4> b;
basic_vec<4> max(basic_vec<4> a) {
  basic_mask __trans_tmp_1 = a < b;
  return __select_impl(__trans_tmp_1, b, a);
}
}
namespace simd = std::simd;
simd::basic_vec test_runner___trans_tmp_2 = max(simd::basic_vec<4>());
