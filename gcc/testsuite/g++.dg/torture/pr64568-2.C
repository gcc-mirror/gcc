// { dg-do compile }

namespace std
{
  typedef __SIZE_TYPE__ size_t;
}
class H;
namespace std
{
  template <typename> struct complex;
  template <typename _Tp>
      complex<_Tp> operator+(complex<_Tp> &__x, complex<_Tp> __y)
	{
	  complex<_Tp> a = __x;
	  a += __y;
	  return a;
	}
  template <> struct complex<double>
    {
      int
	  imag ()
	    {
	      return __imag__ _M_value;
	    }
      void operator+=(complex __z) { _M_value += _M_value; _M_value  += __z.imag (); }
      _Complex double _M_value;
    };
}
struct A
{
  typedef std::complex<double> &const_reference;
};
class B
{
public:
    B (int);
    std::complex<double> &operator[](int i) { return data_[i]; }
    std::complex<double> *data_;
};
struct C
{
  static std::complex<double>
      apply (A::const_reference t1, std::complex<double> t2)
	{
	  return t1 + t2;
	}
  typedef std::complex<double> result_type;
};
template <class T1, class> struct D
{
  static void
      apply (T1 t1, std::complex<double> t2)
	{
	  t1 = t2;
	}
};
class ublas_expression
{
public:
    ~ublas_expression ();
};
template <class> class F
{
};
template <class E> class matrix_expression : ublas_expression
{
public:
    E &operator()() {}
};
class I : public F<int>
{
public:
    typedef int value_type;
    I (int);
};
template <class E1, class E2> matrix_expression<int> outer_prod (F<E1>, F<E2>);
template <class E1, class F> class J : public matrix_expression<J<E1, F> >
{
public:
    typedef typename F::result_type value_type;
    value_type operator()(int i, int)
      {
	return F::apply (e1_ (i, 0), e2_ (0, 0));
      }
    E1 e1_;
    E1 e2_;
};
template <class E1, class E2>
J<H, C> operator+(matrix_expression<E1>, matrix_expression<E2>);
template <template <class, class> class F, class M, class E>
void
indexing_matrix_assign (M m, matrix_expression<E> e, int)
{
  for (int i; i; ++i)
    F<typename M::reference, typename E::value_type>::apply (m (0, 0),
							     e ()(i, 0));
}
template <template <class, class> class F, class, class M, class E, class C>
void
matrix_assign (M m, matrix_expression<E> e, int, C)
{
  indexing_matrix_assign<F> (m, e, 0);
}
template <template <class, class> class F, class M, class E>
void
matrix_assign (M m, matrix_expression<E> e)
{
  matrix_assign<F, int> (m, e, 0, typename M::orientation_category ());
}
class H : matrix_expression<int>
{
public:
    typedef std::complex<double> &reference;
    typedef int orientation_category;
    H (int, int) : data_ (0) {}
    template <class AE> H (matrix_expression<AE> ae) : data_ (0)
  {
    matrix_assign<D> (*this, ae);
  }
    B &
	data ()
	  {
	  }
    std::complex<double> &operator()(int i, int) { return data ()[i]; }
    void operator+=(matrix_expression ae) { H (*this + ae); }
    B data_;
};
template <class M, class T, class V1, class V2>
void
sr2 (M m, T, V1 v1, V2 v2)
{
  m += outer_prod (v2, v1);
}
template <class, class, std::size_t> struct G
{
  void test ();
};
template struct G<I, H, 3>;
template <class V, class M, std::size_t N>
void
G<V, M, N>::test ()
{
  V b (0), c (0);
  M m (0, 0);
  sr2 (m, typename V::value_type (), b, c);
}
