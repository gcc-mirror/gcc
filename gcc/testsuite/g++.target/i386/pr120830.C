/* { dg-do compile } */
/* { dg-options "-O2 -march=znver4 -std=gnu++17" } */

typedef double __m128d __attribute__ ((__vector_size__ (16)));
typedef double __m512d __attribute__ ((__vector_size__ (64)));
enum
{
  ColMajor
};
enum
{
  DefaultProduct
};
template <typename> struct EigenBase
{
};
struct
{
  template <typename OtherDerived>
  void
  operator= (OtherDerived other)
  {
    call_assignment_no_alias (other);
  }
} __trans_tmp_6;
template <typename, typename, int = DefaultProduct> struct Product
{
};
template <typename> struct unpacket_traits;
template <typename Packet>
Packet ploadu (const typename unpacket_traits<Packet>::type *);
template <typename Packet>
Packet pset1 (const typename unpacket_traits<Packet>::type &);
struct Packet1cd
{
  Packet1cd (__m128d a) : v (a) {}
  __m128d v;
};
template <> struct unpacket_traits<Packet1cd>
{
  typedef int type;
};
template <>
Packet1cd
ploadu (const int *from)
{
  return *(
      __attribute__ ((__vector_size__ (2 * sizeof (double)))) double *)from;
}
template <>
Packet1cd
pset1 (const int &from)
{
  return ploadu<Packet1cd> (&from);
}
struct Packet4cd
{
  Packet4cd (__m512d);
};
template <> struct unpacket_traits<Packet4cd>
{
  typedef int type;
};
__m512d pset1___trans_tmp_3;
template <>
Packet4cd
pset1 (const int &from)
{
  Packet1cd __trans_tmp_1 = pset1<Packet1cd> (from);
  __m512d __trans_tmp_2;
  pset1___trans_tmp_3 = __builtin_ia32_broadcastf64x2_512_mask (
      __trans_tmp_1.v, __trans_tmp_2, -1);
  return pset1___trans_tmp_3;
}
template <typename> struct assign_op
{
};
template <typename, typename, typename> struct Assignment;
assign_op<int> call_assignment_no_alias_func;
EigenBase<int> call_assignment_no_alias_dst;
template <typename Src>
void
call_assignment_no_alias (Src src)
{
  Assignment<EigenBase<int>, Src, assign_op<int>>::run (
      call_assignment_no_alias_dst, src, call_assignment_no_alias_func);
}
template <typename, typename, typename, int>
struct general_matrix_matrix_product;
struct blas_data_mapper
{
  blas_data_mapper (int *, int);
};
template <typename LhsScalar, typename RhsScalar, typename Index,
          typename DataMapper>
struct gebp_kernel
{
  typedef LhsScalar ResScalar;
  void operator() (const DataMapper &, const LhsScalar *, const RhsScalar *,
                   Index, Index, Index, ResScalar, Index = 1, Index = 1,
                   Index = 0, Index = 0);
};
struct lhs_process_one_packet
{
  void
  operator() (blas_data_mapper, const int *, const int *, int alpha, long,
              long peelEnd, long, long, long, long, int, long, long, long,
              long, long)
  {
    for (; peelEnd;)
      pset1<Packet4cd> (alpha);
    pset1<Packet4cd> (alpha);
  }
};
template <typename LhsScalar, typename RhsScalar, typename Index,
          typename DataMapper>
__attribute__ ((noinline)) void
gebp_kernel<LhsScalar, RhsScalar, Index, DataMapper>::operator() (
    const DataMapper &res, const LhsScalar *blockA, const RhsScalar *blockB,
    Index rows, Index depth, Index cols, ResScalar alpha, Index, Index, Index,
    Index)
{
  Index packet_cols4, peeled_mc2;
  enum
  {
    pk
  } peeled_kc;
  int prefetch_res_offset;
  lhs_process_one_packet p;
  p (res, blockA, blockB, alpha, peeled_mc2, rows, 1, 1, 0, 0,
     prefetch_res_offset, peeled_kc, pk, cols, depth, packet_cols4);
}
long parallelize_gemm_rows;
template <int, typename Functor>
void
parallelize_gemm (Functor func)
{
  long actualBlockCols;
  func (0, actualBlockCols, 0, parallelize_gemm_rows);
}
template <typename> struct generic_product_impl;
template <typename DstXprType, typename Lhs, typename Rhs, int Options,
          typename Scalar>
struct Assignment<DstXprType, Product<Lhs, Rhs, Options>, assign_op<Scalar>>
{
  static void
  run (DstXprType dst, Product<Lhs, Rhs>, assign_op<Scalar>)
  {
    EigenBase<int> __trans_tmp_4, __trans_tmp_8;
    generic_product_impl<Rhs>::evalTo (dst, __trans_tmp_4, __trans_tmp_8);
  }
};
template <typename Index, typename LhsScalar, typename RhsScalar>
struct general_matrix_matrix_product<Index, LhsScalar, RhsScalar, ColMajor>
{
  typedef LhsScalar ResScalar;
  static void
  run ()
  {
    ResScalar alpha, _res, blockA, blockB;
    Index resIncr, actual_kc;
    blas_data_mapper res (&_res, resIncr);
    gebp_kernel<LhsScalar, RhsScalar, Index, blas_data_mapper> gebp;
    gebp (res, &blockA, &blockB, 0, actual_kc, 0, alpha);
  }
};
template <typename Lhs, typename Rhs, typename BlockingType>
struct gemm_functor
{
  gemm_functor (Lhs, Rhs, EigenBase<int>, double, BlockingType);
  void
  operator() (long, long, long, int)
  {
    general_matrix_matrix_product<long, int, int, 0>::run ();
  }
};
template <typename> struct generic_product_impl
{
  template <typename Dst>
  static void
  evalTo (Dst dst, EigenBase<int>, EigenBase<int>)
  {
    scaleAndAddTo (dst);
  }
  template <typename Dest>
  static void
  scaleAndAddTo (Dest dst)
  {
    int lhs, rhs;
    void blocking ();
    parallelize_gemm<0> (gemm_functor (lhs, rhs, dst, 0, blocking));
  }
};
struct ComplexEigenSolver
{
  template <typename InputType> void compute (const EigenBase<InputType> &);
};
Product<int, int> __trans_tmp_7;
template <typename InputType>
void
ComplexEigenSolver::compute (const EigenBase<InputType> &)
{
  __trans_tmp_6 = __trans_tmp_7;
}
void
compute_eigenvec ()
{
  EigenBase<int> Mtilde_eig_cmplx;
  ComplexEigenSolver ces;
  ces.compute (Mtilde_eig_cmplx);
}
