/* { dg-do compile } */
/* { dg-additional-options "-w" } */
/* { dg-additional-options "-mavx2" { target { x86_64-*-* i?86-*-* } } } */

namespace std {
template < typename _Default > struct __detector { using type = _Default; };
template < typename _Default, template < typename > class >
using __detected_or = __detector< _Default >;
template < typename _Default, template < typename > class _Op >
using __detected_or_t = typename __detected_or< _Default, _Op >::type;
template < typename > struct iterator_traits;
template < typename _Tp > struct iterator_traits< _Tp * > {
  typedef _Tp reference;
};
} // std
using std::iterator_traits;
template < typename _Iterator, typename > struct __normal_iterator {
  typename iterator_traits< _Iterator >::reference operator*();
  void operator++();
};
template < typename _IteratorL, typename _IteratorR, typename _Container >
int operator!=(__normal_iterator< _IteratorL, _Container >,
               __normal_iterator< _IteratorR, _Container >);
namespace std {
template < typename _Tp > struct allocator { typedef _Tp value_type; };
struct __allocator_traits_base {
  template < typename _Tp > using __pointer = typename _Tp::pointer;
};
template < typename _Alloc > struct allocator_traits : __allocator_traits_base {
  using pointer = __detected_or_t< typename _Alloc::value_type *, __pointer >;
};
} // std
typedef double __m128d __attribute__((__vector_size__(16)));
typedef double __m256d __attribute__((__vector_size__(32)));
enum { InnerVectorizedTraversal, LinearVectorizedTraversal };
enum { ReadOnlyAccessors };
template < int, typename Then, typename > struct conditional {
  typedef Then type;
};
template < typename Then, typename Else > struct conditional< 0, Then, Else > {
  typedef Else type;
};
template < typename, typename > struct is_same {
  enum { value };
};
template < typename T > struct is_same< T, T > {
  enum { value = 1 };
};
template < typename > struct traits;
struct accessors_level {
  enum { has_direct_access, has_write_access, value };
};
template < typename > struct EigenBase;
template < typename > struct PlainObjectBase;
template < typename, int = accessors_level::value > struct DenseCoeffsBase;
template < typename, int, int, int = 0, int = 0, int = 0 > struct Matrix;
template < typename > struct MatrixBase;
template < typename, int, int, bool = 0 > struct Block;
struct VectorBlock;
template < typename, typename > struct CwiseNullaryOp;
template < typename, typename, typename > struct CwiseBinaryOp;
template < typename, int = accessors_level::has_write_access > struct MapBase;
template < typename > struct packet_traits;
template < typename > struct unpacket_traits;
template < int Size, typename PacketType,
           int = Size == is_same< PacketType, typename unpacket_traits<
                                                  PacketType >::half >::value >
struct find_best_packet_helper;
template < int Size, typename PacketType >
struct find_best_packet_helper< Size, PacketType, 1 > {
  typedef PacketType type;
};
template < int Size, typename PacketType >
struct find_best_packet_helper< Size, PacketType, 0 > {
  typedef typename find_best_packet_helper<
      1, typename unpacket_traits< PacketType >::half >::type type;
};
template < typename T, int Size > struct find_best_packet {
  typedef typename find_best_packet_helper<
      Size, typename packet_traits< T >::type >::type type;
};
struct compute_matrix_flags {
  enum { ret = 1 };
};
struct ref_selector {
  typedef Matrix< double, 10, 1 > &type;
};
template < typename Derived > struct dense_xpr_base {
  typedef MatrixBase< Derived > type;
};
template < typename ExpressionType > struct is_lvalue {
  enum { value = traits< ExpressionType >::Flags };
};
template < typename Packet > void pmul(Packet);
template < typename Packet >
Packet pload(const typename unpacket_traits< Packet >::type *);
template < typename Packet >
Packet pset1(const typename unpacket_traits< Packet >::type &);
template < typename Scalar, typename Packet > void pstoreu(Scalar, Packet &);
template < typename Packet, int >
Packet ploadt(const typename unpacket_traits< Packet >::type *from) {
  return pload< Packet >(from);
}
template < typename Scalar, typename Packet, int >
void pstoret(Scalar *to, const Packet from) {
  pstoreu(to, from);
}
typedef __m128d Packet2d;
template <> struct unpacket_traits< Packet2d > {
  typedef double type;
  typedef Packet2d half;
};
template <> Packet2d pload(const double *from) { return *(__m128d *)from; }
typedef __m256d Packet4d;
template <> struct packet_traits< double > { typedef Packet4d type; };
template <> struct unpacket_traits< Packet4d > {
  typedef double type;
  typedef Packet2d half;
};
__m256d pset1___trans_tmp_1;
template <> Packet4d pset1(const double &) {
  int __A;
  pset1___trans_tmp_1 = __m256d{__A};
  return pset1___trans_tmp_1;
}
template <> void pstoreu(double *to, const Packet4d &from) {
  *(__attribute__((__vector_size__(4 * sizeof(double)))) double *)to = from;
}
struct scalar_product_op {
  template < typename Packet > void packetOp(Packet a, Packet) { pmul(a); }
};
struct scalar_constant_op {
  template < typename PacketType > PacketType packetOp() {
    return pset1< PacketType >(0);
  }
};
struct assign_op {
  template < int, typename Packet > void assignPacket(double *a, Packet b) {
    pstoret< double, Packet, 0 >(a, b);
  }
};
template < typename Derived >
struct DenseCoeffsBase< Derived, 0 > : EigenBase< Derived > {};
template < typename Derived >
struct DenseCoeffsBase< Derived > : DenseCoeffsBase< Derived, 0 > {};
template < typename Derived > struct DenseBase : DenseCoeffsBase< Derived > {
  using DenseCoeffsBase< Derived >::derived;
  enum { SizeAtCompileTime, MaxSizeAtCompileTime };
  static CwiseNullaryOp< scalar_constant_op, Derived > Constant();
  Derived &setConstant();
  struct FixedSegmentReturnType {
    typedef VectorBlock Type;
  };
  template < int > typename FixedSegmentReturnType::Type segment() {
    return typename FixedSegmentReturnType::Type(derived(), 0, 0);
  }
};
template < typename Derived > struct MatrixBase : DenseBase< Derived > {
  using DenseBase< Derived >::derived;
  template < typename OtherDerived >
  CwiseBinaryOp< scalar_product_op, const Derived, const OtherDerived >
  cwiseProduct(OtherDerived) {
    return CwiseBinaryOp< scalar_product_op, const Derived,
                          const OtherDerived >(derived(), derived());
  }
  template < typename OtherDerived >
  Derived &operator=(const DenseBase< OtherDerived > &);
};
template < typename Derived > struct EigenBase {
  Derived &derived() { return *static_cast< Derived * >(this); }
  Derived derived() const;
};
template < typename > struct binary_evaluator;
template < typename Derived > struct evaluator {
  typedef Derived PlainObjectType;
  typedef typename PlainObjectType::Scalar Scalar;
  enum { IsVectorAtCompileTime, Flags };
  evaluator(PlainObjectType m) : m_data(m.data()) {}
  Scalar &coeffRef(int, int);
  template < int, typename PacketType > PacketType packet(int, int) {
    return ploadt< PacketType, 0 >(m_data);
  }
  const Scalar *m_data;
};
template < typename Scalar, int Rows, int Cols, int Options, int MaxRows,
           int MaxCols >
struct evaluator< Matrix< Scalar, Rows, Cols, Options, MaxRows, MaxCols > >
    : evaluator< PlainObjectBase< Matrix< Scalar, Rows, Cols > > > {
  typedef Matrix< Scalar, Rows, Cols > XprType;
  evaluator(XprType m) : evaluator< PlainObjectBase< XprType > >(m) {}
};
struct nullary_wrapper {
  template < typename T, typename IndexType >
  T packetOp(scalar_constant_op op, IndexType, IndexType) {
    return op.packetOp< T >();
  }
};
template < typename NullaryOp, typename PlainObjectType >
struct evaluator< CwiseNullaryOp< NullaryOp, PlainObjectType > > {
  evaluator(CwiseNullaryOp< NullaryOp, PlainObjectType >);
  template < int, typename PacketType, typename IndexType >
  PacketType packet(IndexType row, IndexType col) {
    return m_wrapper.packetOp< PacketType >(m_functor, row, col);
  }
  NullaryOp m_functor;
  nullary_wrapper m_wrapper;
};
template < typename BinaryOp, typename Lhs, typename Rhs >
struct evaluator< CwiseBinaryOp< BinaryOp, Lhs, Rhs > >
    : binary_evaluator< CwiseBinaryOp< BinaryOp, Lhs, Rhs > > {
  evaluator(CwiseBinaryOp< BinaryOp, Lhs, Rhs > xpr)
      : binary_evaluator< CwiseBinaryOp< BinaryOp, Lhs, Rhs > >(xpr) {}
};
template < typename BinaryOp, typename Lhs, typename Rhs >
struct binary_evaluator< CwiseBinaryOp< BinaryOp, Lhs, Rhs > > {
  binary_evaluator(CwiseBinaryOp< BinaryOp, Lhs, Rhs > xpr)
      : m_lhsImpl(xpr.lhs()), m_rhsImpl(xpr.rhs()) {}
  template < int, typename PacketType > PacketType packet(int, int) {
    PacketType __trans_tmp_1 = m_lhsImpl.template packet< 0, PacketType >(0, 0);
    PacketType __trans_tmp_2;
    m_functor.packetOp(__trans_tmp_1, __trans_tmp_2);
  }
  BinaryOp m_functor;
  evaluator< Lhs > m_lhsImpl;
  evaluator< Rhs > m_rhsImpl;
};
template < typename Derived > struct mapbase_evaluator {
  typedef Derived XprType;
  mapbase_evaluator(XprType map) : m_data(map.data()) {}
  typename XprType::Scalar &coeffRef(int, int) { return m_data[0]; }
  typename XprType::PointerType m_data;
};
template < int > struct block_evaluator;
template < typename ArgType, int BlockRows, int BlockCols, bool InnerPanel >
struct evaluator< Block< ArgType, BlockRows, BlockCols, InnerPanel > >
    : block_evaluator< BlockCols > {
  enum { Flags };
  evaluator(Block< ArgType, 1, 1 > block) : block_evaluator< 1 >(block) {}
};
template < int BlockCols >
struct block_evaluator
    : mapbase_evaluator< Block< Matrix< double, 10, 1 >, 1, BlockCols > > {
  typedef Block< Matrix< double, 10, 1 >, 1, BlockCols > XprType;
  block_evaluator(XprType block) : mapbase_evaluator< XprType >(block) {}
};
template < typename DstEvaluator > struct copy_using_evaluator_traits {
  typedef typename DstEvaluator::XprType Dst;
  typedef typename Dst::Scalar DstScalar;
  enum { DstFlags = DstEvaluator::Flags };
  enum { InnerSize = DstFlags };
  typedef typename conditional<
      int() == LinearVectorizedTraversal,
      typename find_best_packet< DstScalar, Dst::SizeAtCompileTime >::type,
      typename find_best_packet< DstScalar, InnerSize >::type >::type
      PacketType;
};
template < typename Kernel >
struct copy_using_evaluator_innervec_CompleteUnrolling {
  enum { outer, inner, SrcAlignment, DstAlignment };
  static void run(Kernel kernel) {
    kernel.template assignPacketByOuterInner< DstAlignment, SrcAlignment,
                                              typename Kernel::PacketType >(
        outer, inner);
  }
};
template < typename Kernel > struct dense_assignment_loop {
  static void run(Kernel kernel) {
    copy_using_evaluator_innervec_CompleteUnrolling< Kernel >::run(kernel);
  }
};
template < typename DstEvaluatorTypeT, typename SrcEvaluatorTypeT,
           typename Functor >
struct generic_dense_assignment_kernel {
  typedef DstEvaluatorTypeT DstXprType;
  typedef DstEvaluatorTypeT DstEvaluatorType;
  typedef SrcEvaluatorTypeT SrcEvaluatorType;
  typedef typename copy_using_evaluator_traits< DstEvaluatorTypeT >::PacketType
      PacketType;
  generic_dense_assignment_kernel(DstEvaluatorType dst, SrcEvaluatorType src,
                                  Functor, DstXprType dstExpr)
      : m_dst(dst), m_src(src), m_dstExpr(dstExpr) {}
  template < int StoreMode, int LoadMode, typename >
  void assignPacketByOuterInner(long, long) {
    long row;
    long col;
    m_functor.template assignPacket< StoreMode >(
        &m_dst.coeffRef(row, col),
        m_src.template packet< LoadMode, PacketType >(row, col));
  }
  DstEvaluatorType &m_dst;
  SrcEvaluatorType m_src;
  Functor m_functor;
  DstXprType m_dstExpr;
};
template < typename DstXprType, typename SrcXprType, typename Functor >
void call_dense_assignment_loop(DstXprType dst, SrcXprType src, Functor func) {
  typedef evaluator< DstXprType > DstEvaluatorType;
  typedef evaluator< SrcXprType > SrcEvaluatorType;
  SrcEvaluatorType srcEvaluator(src);
  DstEvaluatorType dstEvaluator(dst);
  typedef generic_dense_assignment_kernel< DstEvaluatorType, SrcEvaluatorType,
                                           Functor >
      Kernel;
  Kernel kernel(dstEvaluator, srcEvaluator, func, dst);
  dense_assignment_loop< Kernel >::run(kernel);
}
template < typename, typename, typename > struct Assignment;
template < typename Dst, typename Src > void call_assignment(Dst dst, Src src) {
  call_assignment(dst, src, assign_op());
}
template < typename Dst, typename Src, typename Func >
void call_assignment(Dst dst, Src src, Func func) {
  call_assignment_no_alias(dst, src, func);
}
template < typename Dst, typename Src, typename Func >
void call_assignment_no_alias(Dst dst, Src src, Func func) {
  enum { NeedToTranspose };
  Assignment< typename conditional< NeedToTranspose, int, Dst >::type, Src,
              Func >::run(dst, src, func);
}
template < typename DstXprType, typename SrcXprType, typename Functor >
struct Assignment {
  static void run(DstXprType dst, SrcXprType src, Functor func) {
    call_dense_assignment_loop(dst, src, func);
  }
};
template < typename Derived >
template < typename OtherDerived >
Derived &MatrixBase< Derived >::
operator=(const DenseBase< OtherDerived > &other) {
  call_assignment(derived(), other.derived());
}
template < int Size > struct plain_array { double array[Size]; };
template < int Size > class DenseStorage {
  plain_array< Size > m_data;

public:
  const double *data() const { return m_data.array; }
  double *data() { return m_data.array; }
};
template < typename Derived >
struct PlainObjectBase : dense_xpr_base< Derived >::type {
  typedef typename dense_xpr_base< Derived >::type Base;
  typedef typename traits< Derived >::Scalar Scalar;
  DenseStorage< Base::MaxSizeAtCompileTime > m_storage;
  const Scalar *data() const { return m_storage.data(); }
  Scalar *data() { return m_storage.data(); }
  PlainObjectBase() {}
  template < typename OtherDerived > PlainObjectBase(OtherDerived other) {
    call_assignment_no_alias(this->derived(), other, assign_op());
  }
};
template < typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
           int _MaxCols >
struct traits< Matrix< _Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols > > {
  typedef _Scalar Scalar;
  enum { Flags = compute_matrix_flags::ret };
};
template < typename, int _Rows, int _Cols, int, int, int >
struct Matrix : PlainObjectBase< Matrix< double, _Rows, _Cols > > {
  PlainObjectBase< Matrix > Base;
  Matrix() {}
  template < typename OtherDerived > Matrix(OtherDerived other) : Base(other) {}
};
template < typename, typename, typename > struct CwiseBinaryOp {
  typedef ref_selector::type LhsNested;
  CwiseBinaryOp(Matrix< double, 10, 1 > &aLhs, Matrix< double, 0, 0 >)
      : m_lhs(aLhs) {}
  LhsNested lhs() { return m_lhs; }
  Matrix< double, 8, 1 > rhs() {}
  LhsNested m_lhs;
};
template < typename NullaryOp, typename >
struct CwiseNullaryOp
    : dense_xpr_base< CwiseNullaryOp< NullaryOp, int > >::type {};
template < typename Derived > Derived &DenseBase< Derived >::setConstant() {
  derived() = Constant();
}
template < typename Derived >
struct MapBase< Derived, ReadOnlyAccessors > : dense_xpr_base< Derived >::type {
  typedef typename dense_xpr_base< Derived >::type Base;
  typedef typename traits< Derived >::Scalar Scalar;
  typedef typename conditional< is_lvalue< Derived >::value, Scalar *,
                                Scalar >::type PointerType;
  Scalar *data() { return m_data; }
  MapBase(PointerType dataPtr, long, long) : m_data(dataPtr) {}
  PointerType m_data;
};
template < typename Derived >
struct MapBase< Derived > : MapBase< Derived, ReadOnlyAccessors > {
  typedef MapBase< Derived, ReadOnlyAccessors > Base;
  MapBase(typename Base::PointerType dataPtr, long rows, long cols)
      : Base(dataPtr, rows, cols) {}
  using MapBase< Derived, ReadOnlyAccessors >::Base::operator=;
};
template < typename XprType, int BlockRows, int BlockCols, bool InnerPanel >
struct traits< Block< XprType, BlockRows, BlockCols, InnerPanel > >
    : traits< XprType > {};
template < int, int > struct BlockImpl_dense;
template < typename, int, int, typename > class BlockImpl;
template < typename, int BlockRows, int BlockCols, bool >
struct Block : BlockImpl< Matrix< double, 10, 1 >, BlockRows, BlockCols, int > {
  typedef BlockImpl< Matrix< double, 10, 1 >, BlockRows, BlockCols, int > Impl;
  using Impl::operator=;
  Block(Matrix< double, 10, 1 > &xpr, long startRow, long startCol,
        long blockRows, long blockCols)
      : Impl(xpr, startRow, startCol, blockRows, blockCols) {}
};
template < typename XprType, int BlockRows, int BlockCols >
struct BlockImpl< XprType, BlockRows, BlockCols, int >
    : BlockImpl_dense< BlockRows, BlockCols > {
  typedef BlockImpl_dense< BlockRows, BlockCols > Impl;
  typedef Impl Base;
  using Base::operator=;
  BlockImpl(XprType &xpr, long startRow, long startCol, long blockRows,
            long blockCols)
      : Impl(xpr, startRow, startCol, blockRows, blockCols) {}
};
template < int BlockRows, int BlockCols >
struct BlockImpl_dense
    : MapBase< Block< Matrix< double, 10, 1 >, BlockRows, BlockCols > > {
  typedef MapBase< Block< Matrix< double, 10, 1 >, BlockRows, BlockCols > >
      Base;
  using Base::operator=;
  BlockImpl_dense(Matrix< double, 10, 1 > &xpr, long, long, long blockRows,
                  long blockCols)
      : Base(xpr.data(), blockRows, blockCols) {}
};
struct VectorBlock : Block< int, traits< Matrix< double, 0, 1 > >::Flags, 1 > {
  VectorBlock(Matrix< double, 10, 1 > &vector, long start, long size)
      : Block(vector, 0, start, 1, size) {}
};
namespace std {
template < typename _Alloc > struct _Vector_base {
  typedef typename allocator_traits< _Alloc >::pointer pointer;
};
template < typename _Tp, typename _Alloc = allocator< _Tp > > class vector {
public:
  typedef __normal_iterator< typename _Vector_base< _Alloc >::pointer, int >
      iterator;
  iterator begin();
  iterator end();
};
struct FrameHessian {
  Matrix< double, 0, 1 > step;
  void setState(Matrix< double, 0, 1 >);
};
struct FullSystem {
  bool doStepFromBackup();
  vector< FrameHessian * > frameHessians;
};
bool FullSystem::doStepFromBackup() {
  Matrix< double, 10, 1 > pstepfac;
  pstepfac.segment< 4 >().setConstant();
  for (FrameHessian *fh : frameHessians)
    fh->setState(pstepfac.cwiseProduct(fh->step));
}
} // namespace std
