// PR middle-end/85496
// Reported by Marek Polacek <mpolacek@gcc.gnu.org>

template <typename> class complex;
template <typename _Tp> complex<_Tp> operator*(complex<_Tp>, complex<_Tp>);
template <> struct complex<float> { _Complex float _M_value; };
class A {
  complex<float> _f0, _f1;

public:
  complex<float> &m_fn1() { return _f1; }
};
complex<float> a;
void cos() {
  A b;
  complex<float> c;
  b.m_fn1() = c * a;
}
