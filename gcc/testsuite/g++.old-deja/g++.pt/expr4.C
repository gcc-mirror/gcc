template<class View, class W>
class TinyContainer {
public:

  typedef W T_Wrapped;

  TinyContainer() { }
  TinyContainer(View data) : m_data(data) { }

  T_Wrapped &unwrap() 
  { 
    return *static_cast<T_Wrapped *>(this);
  }
  const T_Wrapped &unwrap() const 
  { 
    return *static_cast<const T_Wrapped *>(this);
  }

protected:

  mutable View m_data;
};

template<class Op, class Left, class Right>
class TinyBinaryExpr :
  public TinyContainer< Op, TinyBinaryExpr<Op, Left, Right> > {
public:

  typedef typename Left::T_Return T_Return;
  typedef TinyBinaryExpr<Op, Left, Right> T_Expr;
  
  T_Expr makeExpr() const { return *this; }

  TinyBinaryExpr(const Op &op, const Left &left, const Right &right)
  : TinyContainer< Op, TinyBinaryExpr<Op, Left, Right> >(op),
    m_left(left), m_right(right)
  { }

  TinyBinaryExpr(const Left &left, const Right &right)
  : m_left(left), m_right(right)
  { }
  
  Op op() const { return m_data; }
  Left left() const { return m_left; }
  Right right() const { return m_right; }

private:

  Left m_left;
  Right m_right;
};

struct OpAdd {

  template<class T1, class T2>
  static T1 apply(const T1 &l, const T2 &r)
  {
    return l + r;
  }

};

template<class V1, class T1, class V2, class T2>
inline TinyBinaryExpr<OpAdd, typename T1::T_Expr, typename T2::T_Expr>
operator+(const TinyContainer<V1,T1>& l, const TinyContainer<V2,T2>& r)
{
  typedef TinyBinaryExpr<OpAdd, typename T1::T_Expr, typename T2::T_Expr> ret;
  return ret(l.unwrap().makeExpr(), r.unwrap().makeExpr());
}


template<class Op, class T1, class T2, class Functor>
inline
typename T1::T_Return
for_each(const TinyBinaryExpr<Op,T1,T2>& node, Functor f)
{
  return Op::apply(for_each(node.left(),f), for_each(node.right(),f));
}

template<class T, unsigned Nrows, unsigned Ncols, unsigned S1, unsigned S2>
class DenseDataView
  : public TinyContainer< T*, DenseDataView<T, Nrows, Ncols, S1, S2> > {
public:

  typedef T T_Return;
  typedef DenseDataView<T, Nrows, Ncols, S1, S2> T_Expr;

  T_Expr makeExpr() const { return *this; }

  T *beginLoc(unsigned i, unsigned j) const 
  { return m_data + S1 * i + S2 * j; }

  DenseDataView(T *pData) 
  : TinyContainer< T*, DenseDataView<T, Nrows, Ncols, S1, S2> >(pData) { }

  T &offset(unsigned i, unsigned j)
  {
    return m_data[S1 * i + S2 * j];
  }

  T offset(unsigned i, unsigned j) const
  {
    return m_data[S1 * i + S2 * j];
  }

  template<unsigned I, unsigned J>
  struct Offset {

    static T &apply(DenseDataView<T, Nrows, Ncols, S1, S2> &d)
    {
      return d.m_data[S1 * I + S2 * J];
    }

    static T constApply(const DenseDataView<T, Nrows, Ncols, S1, S2> &d)
    {
      return d.m_data[S1 * I + S2 * J];
    }

  };

};

template<unsigned I, unsigned J>
struct Eval2 { };

template<class T, unsigned Nrows, unsigned Ncols, unsigned S1, unsigned S2,
  unsigned I, unsigned J>
inline T
for_each(const DenseDataView<T, Nrows, Ncols, S1, S2> &d, 
  const Eval2<I,J> &e)
{
  return d.offset(I, J);
}

template<class T, unsigned Nrows, unsigned Ncols>
class DenseData
  : public TinyContainer< T[Nrows * Ncols], DenseData<T, Nrows, Ncols> > {
public:

  typedef T T_Return;
  typedef DenseDataView<T, Nrows, Ncols, 1, Nrows> T_Expr;

  T_Expr makeExpr() const { return T_Expr(m_data); }

  T *beginLoc(unsigned i, unsigned j) const 
  { return &m_data[i + Nrows * j]; }

  T &operator[](unsigned i)
  {
    return m_data[i];
  }

  T operator[](unsigned i) const
  {
    return m_data[i];
  }

  T &offset(unsigned i, unsigned j)
  {
    return m_data[i + Nrows * j];
  }

  T offset(unsigned i, unsigned j) const
  {
    return m_data[i + Nrows * j];
  }

  template<unsigned I, unsigned J>
  struct Offset {

    static T &apply(DenseData<T, Nrows, Ncols> &d)
    {
      return d.m_data[I + Nrows * J];
    }

    static T constApply(const DenseData<T, Nrows, Ncols> &d)
    {
      return d.m_data[I + Nrows * J];
    }

  };

};

template<class T, unsigned Nrc>
class DiagonalData {
public:

  T &offset(unsigned i, unsigned j)
  {
    assert(i == j);
    return m_data[i];
  }

  T offset(unsigned i, unsigned j) const
  {
    return (i == j) ? m_data[i] : T(0);
  }

  template<unsigned I, unsigned J>
  struct Offset {

    static T &apply(DiagonalData<T,Nrc> &d)
    {
      assert(I == J);
      return d.m_data[I];
    }

    static T constApply(const DiagonalData<T,Nrc> &d)
    {
      return (I == J) ? d.m_data[I] : T(0);
    }

  };

private:
  
  T m_data[Nrc];
};

template<unsigned I, unsigned J, unsigned C1>
struct InnerLoop {

  template<class LHS, class RHS>
  static inline void eval(LHS &l, const RHS &r)
  {
    l.offset(I,J) = for_each(r, Eval2<I,J>());
    InnerLoop<I + 1, J, C1 - 1>::eval(l, r);
  }

};

template<unsigned I, unsigned J>
struct InnerLoop<I, J, 0> {

  template<class LHS, class RHS>
  static inline void eval(LHS &, const RHS &) { }

};

template<unsigned I, unsigned J, unsigned C1, unsigned C2>
struct Loop2 {

  template<class LHS, class RHS>
  static inline void eval(LHS &l, const RHS &r)
  {
    InnerLoop<I, J, C1>::eval(l, r);
    Loop2<I, J + 1, C1, C2 - 1>::eval(l, r);
  }
};

template<unsigned I, unsigned J, unsigned C1>
struct Loop2<I, J, C1, 0> {

  template<class LHS, class RHS>
  static inline void eval(LHS &l, const RHS &r) { }

};


template<unsigned Begin, unsigned End, unsigned Stride = 1>
class TinyRange {
public:

  static const unsigned b = Begin;
  static const unsigned e = End;
  static const unsigned s = Stride;
  static const unsigned n = (End - Begin) / Stride + 1;

  static unsigned index(unsigned i)
    {
      return b + s * i;
    }
};

template<class Range1, class Range2, class Data>
struct Merge { };

template<class Range1, class Range2, class T, unsigned Nrows, unsigned Ncols>
struct Merge<Range1, Range2, DenseData<T, Nrows, Ncols> >
{
  static const unsigned s2 = Nrows * Range2::s;
  typedef 
    DenseDataView<T, Range1::n, Range2::n, Range1::s, s2> type;
};

template<class Range1, class Range2, class T, unsigned Nrows, unsigned Ncols,
  unsigned S1, unsigned S2>
struct Merge<Range1, Range2, DenseDataView<T, Nrows, Ncols, S1, S2> >
{
  static const unsigned s1 = S1 * Range1::s;
  static const unsigned s2 = S2 * Range2::s;

  typedef
    DenseDataView<T, Range1::n, Range2::n, s1, s2> type;
};

template<class T, unsigned Nrows, unsigned Ncols, 
  class Data = DenseData<T, Nrows, Ncols> >
class TinyMatrix :
  public TinyContainer< Data, TinyMatrix<T, Nrows, Ncols, Data> > {
public:

  typedef T T_Return;
  typedef typename Data::T_Expr T_Expr;
  typedef TinyContainer< Data, TinyMatrix<T, Nrows, Ncols, Data> > T_Base;
  
  T_Expr makeExpr() const { return m_data.makeExpr(); }

  TinyMatrix() { }

  TinyMatrix(const T &a0, const T &a1, const T &a2, 
	     const T &a3, const T &a4, const T &a5)
  {
    m_data[0] = a0; m_data[1] = a1; m_data[2] = a2;
    m_data[3] = a3; m_data[4] = a4; m_data[5] = a5;
  }

  TinyMatrix(const T &a0, const T &a1)
  {
    m_data[0] = a0; m_data[1] = a1;
  }

  TinyMatrix(const Data &d) : T_Base(d) { }

  T operator()(unsigned i, unsigned j) const
  {
    return m_data.offset(i, j);
  }

  template<unsigned B1, unsigned E1, unsigned S1,
    unsigned B2, unsigned E2, unsigned S2>
  TinyMatrix<T, TinyRange<B1, E1, S1>::n,
      TinyRange<B2, E2, S2>::n, 
      typename 
      Merge< TinyRange<B1, E1, S1>, TinyRange<B2, E2, S2>, Data>::type>  
  operator()(const TinyRange<B1, E1, S1> &r1, const TinyRange<B2, E2, S2> &r2)
  {
    typedef typename 
      Merge< TinyRange<B1, E1, S1>, TinyRange<B2, E2, S2>, Data>::type
      T_DataType;
    typedef TinyMatrix<T, TinyRange<B1, E1, S1>::n,
      TinyRange<B2, E2, S2>::n, T_DataType> T_RetType;

    return T_RetType(T_DataType(m_data.beginLoc(B1, B2)));
  }

  template<class V1, class T1>
  void operator=(const TinyContainer<V1, T1> &rhs)
  {
    Loop2<0, 0, Nrows, Ncols>::eval(m_data, rhs.unwrap().makeExpr());
  }

};

    
int main()
{
  TinyMatrix<double, 2, 3> a, b(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
    c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), d(0.01, 0.02, 0.03, 0.04, 0.05, 0.06);
  TinyMatrix<double, 1, 2> e, f(17.0, 48.3);

  a = b + c + d;

  a(TinyRange<0,1>(), TinyRange<0,2,2>());
  
  a(TinyRange<0,1>(), TinyRange<0,2,2>())
    (TinyRange<0,0>(), TinyRange<0,1>());
  
  e = f + a(TinyRange<0,1>(), TinyRange<0,2,2>())
    (TinyRange<0,0>(), TinyRange<0,1>());
}

