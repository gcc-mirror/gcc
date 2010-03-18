/* { dg-do compile } */

template < typename > class basic_stringstream;

struct basic_string {
  basic_string();
};

struct ios_base {
  virtual ~ios_base();
};

class ostream:ios_base {};
class istream:virtual ios_base {};

template < typename > struct basic_iostream:public istream, ostream {
  ~basic_iostream () {}
};
extern template class basic_iostream < char >;

template < typename > struct basic_stringstream:public basic_iostream < char > {
    basic_string _M_stringbuf;
    ~basic_stringstream () {}
};
extern template class basic_stringstream < char >;

template < typename > struct AnyMatrixBase;
template < typename, int _Rows, int _Cols, int = _Rows, int = _Cols > class Matrix;
template < typename > class CwiseNullaryOp;

template < typename Derived > struct MatrixBase:public AnyMatrixBase < Derived > {
  typedef CwiseNullaryOp < Derived > ConstantReturnType;
  ConstantReturnType Constant ();
  template < typename > Derived cast ();
  static CwiseNullaryOp < Derived > Random (int);
};

template < typename Derived > struct AnyMatrixBase {
  Derived derived () {}
  Derived & derived () const {}
};

template < typename, int > struct ei_matrix_storage {};

template < typename _Scalar, int, int, int _MaxRows, int _MaxCols > struct Matrix:MatrixBase < Matrix < _Scalar, _MaxRows, _MaxCols > > {
  typedef MatrixBase < Matrix > Base;
  ei_matrix_storage < int, _MaxCols > m_storage;
  Matrix operator= (const Matrix other) {
    _resize_to_match (other);
    lazyAssign (other.derived ());
  }
  template < typename OtherDerived > Matrix lazyAssign (MatrixBase < OtherDerived > other) {
    _resize_to_match (other);
    return Base (other.derived ());
  }
  Matrix ();
  template < typename OtherDerived > Matrix (const MatrixBase < OtherDerived > &other) {
    *this = other;
  }
  template < typename OtherDerived > void _resize_to_match (const MatrixBase < OtherDerived > &) {
    throw 1;
  }
};

template < typename MatrixType > class CwiseNullaryOp:
public MatrixBase < CwiseNullaryOp < MatrixType > > {};

int f()
{
  bool align_cols;
  if (align_cols) {
    basic_stringstream<char> sstr;
    f();
  }
}

template < typename > struct AutoDiffScalar;
template < typename Functor > struct AutoDiffJacobian:Functor {
  AutoDiffJacobian (Functor);
  typedef typename Functor::InputType InputType;
  typedef typename Functor::ValueType ValueType;
  typedef Matrix < int, Functor::InputsAtCompileTime, 1 > DerivativeType;
  typedef AutoDiffScalar < DerivativeType > ActiveScalar;
  typedef Matrix < ActiveScalar, Functor::InputsAtCompileTime, 1 > ActiveInput;
  void operator () (InputType x, ValueType *) {
    ActiveInput ax = x.template cast < ActiveScalar > ();
  }
};

template < int NX, int NY > struct TestFunc1 {
  enum  {
    InputsAtCompileTime = NX
  };
  typedef Matrix < float, NX, 1 > InputType;
  typedef Matrix < float, NY, 1 > ValueType;
  typedef Matrix < float, NY, NX > JacobianType;
  int inputs ();
};

template < typename Func > void forward_jacobian (Func f) {
  typename Func::InputType x = Func::InputType::Random (f.inputs ());
  typename Func::ValueType y;
  typename Func::JacobianType jref = jref.Constant ();
  AutoDiffJacobian < Func > autoj (f);
  autoj (x, &y);
}

void test_autodiff_scalar ()
{
  forward_jacobian (TestFunc1 < 2, 2 > ());
  forward_jacobian (TestFunc1 < 3, 2 > ());
}
