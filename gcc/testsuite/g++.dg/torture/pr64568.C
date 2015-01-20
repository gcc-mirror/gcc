// { dg-do compile }
// { dg-additional-options "-std=c++11" }

namespace std
{
typedef long unsigned size_t;
template <typename> class complex;
template <typename _Tp> complex<_Tp> operator+(complex<_Tp>, complex<_Tp>)
{
  complex<_Tp> a = 0;
  a += 0;
  return a;
}
template <> struct complex<double>
{
  complex (int __i) : _M_value{ __i } {}
  int imag ();
  void operator+=(complex __z) { _M_value = __z.imag (); }
  _Complex double _M_value;
};
}
class A
{
public:
  A (int);
  std::complex<double> &operator[](int i) { return data_[i]; }
  std::complex<double> *data_;
};
struct B
{
  static std::complex<double>
  apply (std::complex<double> t1, std::complex<double> t2)
  {
    return t1 + t2;
  }
};
template <class T1, class> struct C
{
  static void
  apply (T1 t1, std::complex<double> t2)
  {
    t1 = t2;
  }
};
template <class E> class D
{
public:
  E operator()();
};
class G : public D<G>
{
public:
  typedef std::complex<double> value_type;
  value_type operator()(int) { return B::apply (0, 0); }
};
template <class E1, class E2> G operator+(D<E1>, D<E2>);
template <template <class, class> class F, class V, class E>
void
indexing_vector_assign (V v, D<E> e)
{
  for (int i;; ++i)
    F<typename V::reference, typename E::value_type>::apply (v (i), e ()(0));
}
template <template <class, class> class F, class V, class E>
void
vector_assign (V v, D<E> e, int)
{
  indexing_vector_assign<F> (v, e);
}
template <template <class, class> class F, class V, class E>
void
vector_assign (V v, D<E> e)
{
  vector_assign<F> (v, e, typename V::storage_category ());
}
class H : public D<int>
{
public:
  typedef std::complex<double> &reference;
  typedef int storage_category;
  H (int);
  template <class AE> H (D<AE> ae) : data_ (0)
  {
    vector_assign<C> (*this, ae);
  }
  A
  data ()
  {
    return data_;
  }
  reference operator()(int i) { return data ()[i]; }
  A data_;
};
template <class T1, class V1, class T2, class V2>
void
rot (T1, V1 v1, T2, V2 v2)
{
  H (v1 + v2);
}
template <class, unsigned long> struct F
{
  void test ();
};
template struct F<H, 3>;
template <class V, std::size_t N>
void
F<V, N>::test ()
{
  V b (0), c (0);
  rot (0, b, 0, c);
}
