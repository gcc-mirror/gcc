// PR c++/100102
// { dg-do compile { target c++11 } }

template<int> struct ratio;
template<class T> struct duration {
  static constexpr int _S_gcd();
  template<class> using __is_harmonic = ratio<(duration::_S_gcd)()>;
  using type = __is_harmonic<int>;
};
