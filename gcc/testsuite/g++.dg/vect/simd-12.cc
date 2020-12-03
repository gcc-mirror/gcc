/* { dg-do compile } */
/* { dg-additional-options "-ffast-math -g" } */

template <typename> class complex;
template <typename _Tp> complex<_Tp> operator+(complex<_Tp>, complex<_Tp> __y) {
  complex<_Tp> __r;
  __r += __y;
  return __r;
}
template <typename _Tp> complex<_Tp> operator*(complex<_Tp>, complex<_Tp> __y) {
  complex<_Tp> __r;
  __r *= __y;
  return __r;
}
template <> class complex<double> {
public:
  void operator+=(complex __z) { _M_value += __z.__rep(); }
  void operator*=(complex __z) {
    _Complex __t = __z.__rep();
    _M_value *= __t;
  }
  _Complex __rep() { return _M_value; }
  _Complex _M_value;
};
template <typename> class Vector {
  void equ();
  complex<double> *val;
};
template <typename Number> void Vector<Number>::equ() {
  Number c;
  for (int i; i; ++i) {
    complex<double> __trans_tmp_2 = c * val[i];
    val[i] = val[i] + __trans_tmp_2;
  }
}
template class Vector<complex<double> >;
