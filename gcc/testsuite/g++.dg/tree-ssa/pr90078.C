// { dg-do compile }
// { dg-options "-std=c++14 -O2 -ftemplate-depth=1000000" }

template <class T, int Dim0, int Dim1, int Dim2> struct Tensor3;
template <class A, class T, int Dim0, int Dim1, int Dim2, char i, char j,
          char k>
struct Tensor3_Expr;

template <class T, int Dim0, int Dim1, int Dim2, int Dim3> struct Tensor4;
template <class A, class T, int Dim0, int Dim1, int Dim2, int Dim3, char i,
          char j, char k, char l>
struct Tensor4_Expr;

template <char i, int Dim> struct Index
{};
template <const int N> struct Number
{
  Number(){};
  operator int() const { return N; }
};

template <class T, int Tensor_Dim0, int Tensor_Dim1, int Tensor_Dim2>
struct Tensor3
{
  T data[Tensor_Dim0][Tensor_Dim1][Tensor_Dim2];

  T operator()(const int N1, const int N2, const int N3) const
  {
    return data[N1][N2][N3];
  }

  template <char i, char j, char k, int Dim0, int Dim1, int Dim2>
  Tensor3_Expr<const Tensor3<T, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2>, T,
               Dim0, Dim1, Dim2, i, j, k>
  operator()(const Index<i, Dim0>, const Index<j, Dim1>,
             const Index<k, Dim2>) const
  {
    return Tensor3_Expr<const Tensor3<T, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2>,
                        T, Dim0, Dim1, Dim2, i, j, k>(*this);
  }
};

template <class A, class T, int Dim0, int Dim1, int Dim2, char i, char j,
          char k>
struct Tensor3_Expr
{
  A iter;

  Tensor3_Expr(const A &a) : iter(a) {}
  T operator()(const int N1, const int N2, const int N3) const
  {
    return iter(N1, N2, N3);
  }
};

template <class A, class T, int Tensor_Dim0, int Tensor_Dim1, int Tensor_Dim2,
          int Dim0, int Dim1, int Dim2, char i, char j, char k>
struct Tensor3_Expr<Tensor3<A, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2>, T, Dim0,
                   Dim1, Dim2, i, j, k>
{
  Tensor3<A, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2> &iter;

  Tensor3_Expr(Tensor3<A, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2> &a) : iter(a)
  {}
  T operator()(const int N1, const int N2, const int N3) const
  {
    return iter(N1, N2, N3);
  }
};

template <class A, class B, class T, class U, int Dim0, int Dim1, int Dim23,
          int Dim4, int Dim5, char i, char j, char k, char l, char m>
struct Tensor3_times_Tensor3_21
{
  Tensor3_Expr<A, T, Dim0, Dim1, Dim23, i, j, k> iterA;
  Tensor3_Expr<B, U, Dim23, Dim4, Dim5, k, l, m> iterB;

  template <int CurrentDim>
  T eval(const int N1, const int N2, const int N3, const int N4,
         const Number<CurrentDim> &) const
  {
    return iterA(N1, N2, CurrentDim - 1) * iterB(CurrentDim - 1, N3, N4)
           + eval(N1, N2, N3, N4, Number<CurrentDim - 1>());
  }
  T eval(const int N1, const int N2, const int N3, const int N4,
         const Number<1> &) const
  {
    return iterA(N1, N2, 0) * iterB(0, N3, N4);
  }

  Tensor3_times_Tensor3_21(
    const Tensor3_Expr<A, T, Dim0, Dim1, Dim23, i, j, k> &a,
    const Tensor3_Expr<B, U, Dim23, Dim4, Dim5, k, l, m> &b)
      : iterA(a), iterB(b)
  {}
  T operator()(const int &N1, const int &N2, const int &N3,
               const int &N4) const
  {
    return eval(N1, N2, N3, N4, Number<Dim23>());
  }
};

template <class A, class B, class T, class U, int Dim0, int Dim1, int Dim23,
          int Dim4, int Dim5, char i, char j, char k, char l, char m>
Tensor4_Expr<Tensor3_times_Tensor3_21<A, B, T, U, Dim0, Dim1, Dim23, Dim4,
                                      Dim5, i, j, k, l, m>,
             T, Dim0, Dim1, Dim4, Dim5, i, j, l, m>
operator*(const Tensor3_Expr<A, T, Dim0, Dim1, Dim23, i, j, k> &a,
          const Tensor3_Expr<B, U, Dim23, Dim4, Dim5, k, l, m> &b)
{
  using TensorExpr = Tensor3_times_Tensor3_21<A, B, T, U, Dim0, Dim1, Dim23,
                                              Dim4, Dim5, i, j, k, l, m>;
  return Tensor4_Expr<TensorExpr, T, Dim0, Dim1, Dim4, Dim5, i, j, l, m>(
    TensorExpr(a, b));
};

template <class T, int Tensor_Dim0, int Tensor_Dim1, int Tensor_Dim2,
          int Tensor_Dim3>
struct Tensor4
{
  T data[Tensor_Dim0][Tensor_Dim1][Tensor_Dim2][Tensor_Dim3];

  Tensor4() {}
  T &operator()(const int N1, const int N2, const int N3, const int N4)
  {
    return data[N1][N2][N3][N4];
  }

  template <char i, char j, char k, char l, int Dim0, int Dim1, int Dim2,
            int Dim3>
  Tensor4_Expr<Tensor4<T, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2, Tensor_Dim3>,
               T, Dim0, Dim1, Dim2, Dim3, i, j, k, l>
  operator()(const Index<i, Dim0>, const Index<j, Dim1>, const Index<k, Dim2>,
             const Index<l, Dim3>)
  {
    return Tensor4_Expr<
      Tensor4<T, Tensor_Dim0, Tensor_Dim1, Tensor_Dim2, Tensor_Dim3>, T, Dim0,
      Dim1, Dim2, Dim3, i, j, k, l>(*this);
  };
};

template <class A, class T, int Dim0, int Dim1, int Dim2, int Dim3, char i,
          char j, char k, char l>
struct Tensor4_Expr
{
  A iter;

  Tensor4_Expr(const A &a) : iter(a) {}
  T operator()(const int N1, const int N2, const int N3, const int N4) const
  {
    return iter(N1, N2, N3, N4);
  }
};

template <class A, class T, int Dim0, int Dim1, int Dim2, int Dim3, char i,
          char j, char k, char l>
struct Tensor4_Expr<Tensor4<A, Dim0, Dim1, Dim2, Dim3>, T, Dim0, Dim1, Dim2,
                   Dim3, i, j, k, l>
{
  Tensor4<A, Dim0, Dim1, Dim2, Dim3> &iter;

  Tensor4_Expr(Tensor4<A, Dim0, Dim1, Dim2, Dim3> &a) : iter(a) {}
  T operator()(const int N1, const int N2, const int N3, const int N4) const
  {
    return iter(N1, N2, N3, N4);
  }

  template <class B, class U, int Dim1_0, int Dim1_1, int Dim1_2, int Dim1_3,
            char i_1, char j_1, char k_1, char l_1>
  auto &operator=(const Tensor4_Expr<B, U, Dim1_0, Dim1_1, Dim1_2, Dim1_3, i_1,
                                     j_1, k_1, l_1> &rhs)
  {
    for(int ii = 0; ii < Dim0; ++ii)
      for(int jj = 0; jj < Dim1; ++jj)
        for(int kk = 0; kk < Dim2; ++kk)
          for(int ll = 0; ll < Dim3; ++ll)
            {
              iter(ii, jj, kk, ll) = rhs(ii, jj, kk, ll);
            }
    return *this;
  }
};

int main()
{
  Tensor3<float, 100, 100, 1000> t1;
  Tensor3<float, 1000, 100, 100> t2;

  Index<'l', 100> l;
  Index<'m', 100> m;
  Index<'k', 1000> k;
  Index<'n', 100> n;
  Index<'o', 100> o;

  Tensor4<float, 100, 100, 100, 100> res;
  res(l, m, n, o) = t1(l, m, k) * t2(k, n, o);
  return 0;
}

