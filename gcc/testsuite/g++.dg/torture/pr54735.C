// { dg-do compile }

class Gmpfr
{};
class M : Gmpfr
{
public:
  Gmpfr infconst;
  M(int);
};
template<typename>struct A;
template<typename, int, int, int = 0 ? : 0, int = 0, int = 0>class N;
template<typename>class O;
template<typename>struct B;
struct C
{
  enum
  { value };
};
class D
{
public:
  enum
  { ret };
};
struct F
{
  enum
  { ret = 0 ? : 0 };
};
template<typename Derived>struct G
{
  typedef O<Derived>type;
};
struct H
{
  void operator * ();
};
struct I
{
  enum
  { RequireInitialization = C::value ? : 0, ReadCost };
};
template<typename Derived>struct J
{
  enum
  { ret = A<Derived>::InnerStrideAtCompileTime };
};
template<typename Derived>struct K
{
  enum
  { ret = A<Derived>::OuterStrideAtCompileTime };
};
template<typename Derived>class P : H
{
public:
  using H::operator *;
  typedef typename A<Derived>::Scalar Scalar;
  enum
  { RowsAtCompileTime                                        =
      A<Derived>::RowsAtCompileTime, ColsAtCompileTime       =
      A<Derived>::ColsAtCompileTime, SizeAtCompileTime       =
      F::ret, MaxRowsAtCompileTime                           =
      A<Derived>::MaxRowsAtCompileTime, MaxColsAtCompileTime =
      A<Derived>::MaxColsAtCompileTime, MaxSizeAtCompileTime =
      F::ret, Flags                                          =
      A<Derived>::Flags ? : 0 ? : 0, CoeffReadCost           =
      A<Derived>::CoeffReadCost, InnerStrideAtCompileTime    =
      J<Derived>::ret, OuterStrideAtCompileTime              = K<Derived>::ret };
  B<Derived> operator << (const Scalar&);
};

template<typename Derived>class O : public P<Derived>
{};

template<int _Cols>class L
{
public:

  int cols()
  {
    return _Cols;
  }
};
template<typename Derived>class Q : public G<Derived>::type
{
public:
  typedef typename G<Derived>::type   Base;
  typedef typename A<Derived>::Index  Index;
  typedef typename A<Derived>::Scalar Scalar;
  L<Base::ColsAtCompileTime> m_storage;
  Index cols()
  {
    return m_storage.cols();
  }

  Scalar& coeffRef(Index,
                   Index);
};

template<typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
         int _MaxCols>struct A<N<_Scalar, _Rows, _Cols, _Options, _MaxRows,
                                 _MaxCols> >
{
  typedef _Scalar Scalar;
  typedef int     Index;
  enum
  { RowsAtCompileTime, ColsAtCompileTime                              =
      _Cols, MaxRowsAtCompileTime, MaxColsAtCompileTime, Flags        =
      D::ret, CoeffReadCost                                           =
      I::ReadCost, InnerStrideAtCompileTime, OuterStrideAtCompileTime =
      0 ? : 0 };
};
template<typename _Scalar, int, int _Cols, int, int,
         int>class N : public Q<N<_Scalar, 0, _Cols> >
{
public:
  Q<N> Base;
  template<typename T0, typename T1>N(const T0&,
                                      const T1&);
};
void
__assert_fail(int)
throw() __attribute__((__noreturn__));
template<typename XprType>struct B
{
  typedef typename XprType::Scalar Scalar;
  typedef typename XprType::Index  Index;
  B(XprType & p1, const Scalar &) : m_xpr(p1), m_col(),
                                    m_currentBlockRows(1)
  {} B& operator, (const Scalar&)
  {
    Index a;

    if (m_col == m_xpr.cols())
    {
      m_col              = 0;
      m_currentBlockRows = 1;
      a && "Too       " ? static_cast<void>(0) : __assert_fail(0);
    }
    m_col < m_xpr.cols()
    && "Too       " ? static_cast<void>(0) : __assert_fail(1);
    m_currentBlockRows ? static_cast<void>(0) : __assert_fail(4);
    m_xpr.coeffRef(0, m_col++) = 0;
    return *this;
  }
  ~B()
  {
    1 + m_currentBlockRows && m_col
    && "Too       " ? static_cast<void>(0) : __assert_fail(0);
  }

  XprType& m_xpr;
  Index    m_col;
  Index    m_currentBlockRows;
};

template<typename Derived>B<Derived>P<
  Derived >::operator << (const Scalar&)
{
    return B<Derived>(*static_cast<Derived *>(this), 0);
}

template<class NT, int s>void
               check_()
{
    N<NT, 0, s>m(0, 0);
    m << 0, 0, 0, 0;
}

template<class NT>void check()
{
    check_<NT, 3>();
}

int main()
{
    check<M>();
}
