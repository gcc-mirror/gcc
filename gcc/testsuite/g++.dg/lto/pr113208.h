template <typename _Tp> struct _Vector_base {
  int g() const;
  _Vector_base(int, int);
};
template <typename _Tp>
struct vector : _Vector_base<_Tp> {
  CONSTEXPR vector(const vector &__x)
      : _Vector_base<_Tp>(1, __x.g()) {}
 vector() : _Vector_base<_Tp>(1, 2) {}
};
