/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern void __assert_fail(const char*, const char*, int, const char*);
namespace Eigen {
enum { AutoAlign };
template <int, typename>
struct conditional;
template <typename Else>
struct conditional<false, Else> {
  typedef Else type;
};
template <typename T>
struct remove_reference {
  typedef T type;
};
struct is_arithmetic {
  enum { value };
};
template <typename>
struct traits;
template <typename T>
struct traits<const T> : traits<T> {};
template <typename>
struct evaluator;
template <typename>
struct EigenBase;
template <typename>
class PlainObjectBase;
template <typename, int _Rows, int _Cols, int = AutoAlign, int = _Rows,
          int = _Cols>
class Matrix;
template <typename>
class MatrixBase;
template <typename, typename>
class CwiseNullaryOp;
template <typename, typename, typename>
class CwiseBinaryOp;
template <typename>
struct scalar_constant_op;
template <int _Rows>
struct size_at_compile_time {
  enum { ret = _Rows };
};
struct ref_selector {
  typedef const Matrix<float, 3, 1>& type;
};
template <typename Derived>
struct dense_xpr_base {
  typedef MatrixBase<Derived> type;
};
template <typename Derived, typename = typename traits<Derived>::XprKind>
struct generic_xpr_base {
  typedef typename dense_xpr_base<Derived>::type type;
};
template <typename Expr, typename Scalar = typename Expr::Scalar>
struct plain_constant_type {
  ;
  typedef CwiseNullaryOp<scalar_constant_op<Scalar>,
                         Matrix<Scalar, traits<Expr>::ColsAtCompileTime,
                                traits<Expr>::MaxRowsAtCompileTime,
                                traits<Expr>::MaxColsAtCompileTime>>
      type;
};
struct scalar_product_op {
  float operator()(float a, float b) { return a * b; }
};
template <typename>
struct scalar_constant_op {
  scalar_constant_op(float other) : m_other(other) {}
  float operator()() { return m_other; }
  float m_other;
};
struct assign_op {
  void assignCoeff(float& a, float b) { a = b; }
};
template <typename Derived>
class DenseCoeffsBase : public EigenBase<Derived> {
 public:
  typedef typename traits<Derived>::Scalar Scalar;
  typedef
      typename conditional<is_arithmetic::value, Scalar>::type CoeffReturnType;
};
template <typename Derived>
class DenseBase : public DenseCoeffsBase<Derived> {
 public:
  enum {
    RowsAtCompileTime = traits<Derived>::RowsAtCompileTime,
    SizeAtCompileTime = size_at_compile_time<RowsAtCompileTime>::ret,
    MaxSizeAtCompileTime
  };
};
template <typename Derived>
class MatrixBase : public DenseBase<Derived> {
 public:
  using DenseBase<Derived>::derived;
  template <typename T>
  CwiseBinaryOp<scalar_product_op, const Derived,
                const typename plain_constant_type<Derived, T>::type>
  operator*(T& scalar) {
    return CwiseBinaryOp<scalar_product_op, const Derived,
                         const typename plain_constant_type<Derived>::type>(
        derived(), typename plain_constant_type<Derived>::type(derived().rows(),
                                                               0, scalar));
  }
};
template <typename Derived>
struct EigenBase {
  const Derived& derived() const { return *static_cast<const Derived*>(this); }
  Derived& const_cast_derived() const {
    return *static_cast<Derived*>(const_cast<EigenBase*>(this));
  }
};
template <typename>
struct binary_evaluator;
template <typename T>
struct evaluator<const T> : evaluator<T> {
  evaluator(const T& xpr) : evaluator<T>(xpr) {}
};
template <typename Derived>
struct evaluator {
  typedef Derived PlainObjectType;
  typedef typename PlainObjectType::Scalar Scalar;
  evaluator(const PlainObjectType& m) : m_data(m.data()) {}
  typename PlainObjectType::CoeffReturnType coeff(long row, long) {
    return m_data[row];
  }
  Scalar& coeffRef(long row, long) { return const_cast<Scalar*>(m_data)[row]; }
  const Scalar* m_data;
};
template <typename Scalar, int Rows, int Cols, int Options, int MaxRows,
          int MaxCols>
struct evaluator<Matrix<Scalar, Rows, Cols, Options, MaxRows, MaxCols>>
    : evaluator<PlainObjectBase<Matrix<Scalar, Rows, Cols>>> {
  typedef Matrix<Scalar, Rows, Cols> XprType;
  evaluator(const XprType& m) : evaluator<PlainObjectBase<XprType>>(m) {}
};
struct nullary_wrapper {
  template <typename IndexType>
  float operator()(scalar_constant_op<float> op, IndexType, IndexType) const {
    return op();
  }
};
template <typename NullaryOp, typename PlainObjectType>
struct evaluator<CwiseNullaryOp<NullaryOp, PlainObjectType>> {
  typedef CwiseNullaryOp<NullaryOp, PlainObjectType> XprType;
  evaluator(XprType n) : m_functor(n.functor()) {}
  template <typename IndexType>
  typename XprType::CoeffReturnType coeff(IndexType row, IndexType col) {
    return m_wrapper(m_functor, row, col);
  }
  NullaryOp m_functor;
  nullary_wrapper m_wrapper;
};
template <typename BinaryOp, typename Lhs, typename Rhs>
struct evaluator<CwiseBinaryOp<BinaryOp, Lhs, Rhs>>
    : binary_evaluator<CwiseBinaryOp<BinaryOp, Lhs, Rhs>> {
  evaluator(CwiseBinaryOp<BinaryOp, Lhs, Rhs> xpr)
      : binary_evaluator<CwiseBinaryOp<BinaryOp, Lhs, Rhs>>(xpr) {}
};
template <typename BinaryOp, typename Lhs, typename Rhs>
struct binary_evaluator<CwiseBinaryOp<BinaryOp, Lhs, Rhs>> {
  typedef CwiseBinaryOp<BinaryOp, Lhs, Rhs> XprType;
  binary_evaluator(XprType xpr) : m_lhsImpl(xpr.lhs()), m_rhsImpl(xpr.rhs()) {}
  typename XprType::CoeffReturnType coeff(long row, long col) {
    return m_functor(m_lhsImpl.coeff(row, col), m_rhsImpl.coeff(row, col));
  }
  BinaryOp m_functor;
  evaluator<Lhs> m_lhsImpl;
  evaluator<Rhs> m_rhsImpl;
};
template <typename Kernel, int Index, int Stop>
struct copy_using_evaluator_DefaultTraversal_CompleteUnrolling {
  enum { outer, inner = Index };
  static void run(Kernel kernel) {
    kernel.assignCoeffByOuterInner(outer, inner);
    copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, Index + 1,
                                                            Stop>::run(kernel);
  }
};
template <typename Kernel, int Stop>
struct copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, Stop,
                                                               Stop> {
  static void run(Kernel) {}
};
template <typename Kernel>
struct dense_assignment_loop {
  static void run(Kernel kernel) {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    enum { size = DstXprType::SizeAtCompileTime, alignedSize = 0 };
    copy_using_evaluator_DefaultTraversal_CompleteUnrolling<Kernel, alignedSize,
                                                            size>::run(kernel);
  }
};
template <typename DstEvaluatorTypeT, typename SrcEvaluatorTypeT,
          typename Functor>
class generic_dense_assignment_kernel {
  typedef typename DstEvaluatorTypeT::XprType DstXprType;

 public:
  typedef DstEvaluatorTypeT DstEvaluatorType;
  typedef SrcEvaluatorTypeT SrcEvaluatorType;
  generic_dense_assignment_kernel(DstEvaluatorType dst, SrcEvaluatorType src,
                                  Functor, DstXprType& dstExpr)
      : m_dst(dst), m_src(src), m_dstExpr(dstExpr) {}
  long assignCoeff_col;
  void assignCoeffByOuterInner(long, long inner) {
    long __trans_tmp_1 = inner;
    m_functor.assignCoeff(m_dst.coeffRef(__trans_tmp_1, assignCoeff_col),
                          m_src.coeff(__trans_tmp_1, assignCoeff_col));
  }
  DstEvaluatorType m_dst;
  SrcEvaluatorType m_src;
  Functor m_functor;
  DstXprType& m_dstExpr;
};
template <typename DstXprType, typename SrcXprType, typename Functor>
void call_dense_assignment_loop(DstXprType& dst, SrcXprType src, Functor func) {
  typedef evaluator<DstXprType> DstEvaluatorType;
  typedef evaluator<SrcXprType> SrcEvaluatorType;
  SrcEvaluatorType srcEvaluator(src);
  DstEvaluatorType dstEvaluator(dst);
  typedef generic_dense_assignment_kernel<DstEvaluatorType, SrcEvaluatorType,
                                          Functor>
      Kernel;
  Kernel kernel(dstEvaluator, srcEvaluator, func, dst.const_cast_derived());
  dense_assignment_loop<Kernel>::run(kernel);
}
template <typename Dst, typename Src, typename Func>
void call_assignment_no_alias(Dst& dst, Src src, Func func) {
  enum { NeedToTranspose };
  typename conditional<NeedToTranspose, Dst&>::type actualDst(dst);
  CwiseBinaryOp<scalar_product_op, const Matrix<float, 3, 1>,
                const CwiseNullaryOp<scalar_constant_op<float>,
                                     const Matrix<float, 3, 1, 0, 2, 3>>>
      __trans_tmp_4 = src;
  call_dense_assignment_loop(actualDst, __trans_tmp_4, func);
}
template <int Size>
struct plain_array {
  float array[Size];
};
template <int Size, int _Rows>
class DenseStorage {
  plain_array<Size> m_data;

 public:
  DenseStorage() {}
  DenseStorage(const DenseStorage&);
  static long rows() { return _Rows; }
  const float* data() const { return m_data.array; }
  float* data() { return m_data.array; }
};
template <typename Derived>
class PlainObjectBase : public dense_xpr_base<Derived>::type {
 public:
  typedef typename dense_xpr_base<Derived>::type Base;
  typedef typename traits<Derived>::Scalar Scalar;
  DenseStorage<Base::MaxSizeAtCompileTime, Base::RowsAtCompileTime> m_storage;
  long rows() const { return m_storage.rows(); }
  const Scalar* data() const { return m_storage.data(); }
  PlainObjectBase() {}
  template <typename OtherDerived>
  PlainObjectBase(const DenseBase<OtherDerived>& other) {
    _set_noalias(other);
  }
  template <typename OtherDerived>
  void _set_noalias(const DenseBase<OtherDerived>& other) {
    call_assignment_no_alias(this->derived(), other.derived(), assign_op());
  }
};
template <typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
          int _MaxCols>
struct traits<Matrix<_Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols>> {
  typedef _Scalar Scalar;
  typedef int XprKind;
  enum {
    RowsAtCompileTime = _Rows,
    ColsAtCompileTime,
    MaxRowsAtCompileTime,
    MaxColsAtCompileTime,
  };
};
template <typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
          int _MaxCols>
class Matrix
    : public PlainObjectBase<
          Matrix<_Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols>> {
 public:
  typedef PlainObjectBase<Matrix> Base;
  typedef typename traits<Matrix>::Scalar Scalar;
  Matrix(Scalar& x, Scalar& y, Scalar& z) {
    m_storage.data()[0] = x;
    m_storage.data()[1] = y;
    m_storage.data()[2] = z;
  }
  template <typename OtherDerived>
  Matrix(const EigenBase<OtherDerived>& other) : Base(other.derived()) {}
  using Base::m_storage;
};
template <typename BinaryOp, typename Lhs, typename Rhs>
struct traits<CwiseBinaryOp<BinaryOp, Lhs, Rhs>> {
  typedef typename traits<Lhs>::XprKind XprKind;
  enum { RowsAtCompileTime };
  typedef float Scalar;
};
template <typename>
class CwiseBinaryOpImpl;
template <typename, typename, typename RhsType>
class CwiseBinaryOp : public CwiseBinaryOpImpl<RhsType> {
 public:
  typedef ref_selector::type LhsNested;
  typedef RhsType RhsNested;
  CwiseBinaryOp(const Matrix<float, 3, 1>& aLhs, RhsType& aRhs)
      : m_lhs(aLhs), m_rhs(aRhs) {}
  remove_reference<LhsNested>::type& lhs() { return m_lhs; }
  typename remove_reference<RhsNested>::type& rhs() { return m_rhs; }
  LhsNested m_lhs;
  RhsNested m_rhs;
};
template <typename>
class CwiseBinaryOpImpl
    : public generic_xpr_base<CwiseBinaryOp<
          scalar_product_op, const Matrix<float, 3, 1>,
          const CwiseNullaryOp<scalar_constant_op<float>,
                               const Matrix<float, 3, 1, 0, 2, 3>>>>::type {};
template <typename NullaryOp, typename PlainObjectType>
struct traits<CwiseNullaryOp<NullaryOp, PlainObjectType>>
    : traits<PlainObjectType> {};
template <typename, typename PlainObjectType>
class CwiseNullaryOp
    : public dense_xpr_base<CwiseNullaryOp<int, PlainObjectType>>::type {
 public:
  CwiseNullaryOp(long rows, long, scalar_constant_op<float> func)
      : m_functor(func) {
    rows ? void() : __assert_fail("", "", 1, __PRETTY_FUNCTION__);
  }
  scalar_constant_op<float> functor() { return m_functor; }
  scalar_constant_op<float> m_functor;
};
}  // namespace Eigen
Eigen::Matrix<float, 3, 1> should_inline(float x, float y, float z,
                                         float scale) {
  return Eigen::Matrix<float, 3, 1>(x, y, z) * scale;
}

// We should inline everything to should_inline

/* { dg-final { scan-tree-dump-times "Function" "optimized" } } */
