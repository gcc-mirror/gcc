/* PR tree-optimization/112822 */
/* { dg-do compile { target c++17 } } */
/* { dg-options "-w -O2" } */

/* Verify we do not ICE on the following noisy creduced test case.  */

namespace b {
typedef int c;
template <bool, typename> struct d;
template <typename e> struct d<true, e> { using f = e; };
template <bool, typename, typename> struct aa;
template <typename g, typename h> struct aa<false, g, h> { using f = h; };
template <bool l, typename e> using ab = typename d<l, e>::f;
template <bool l, typename g, typename h> using n = typename aa<l, g, h>::f;
template <typename> class af {
public:
  typedef __complex__ ah;
  template <typename e> af operator+=(e) {
    ah o;
    x = o;
    return *this;
  }
  ah x;
};
} // namespace b
namespace {
enum { p };
enum { ac, ad };
struct ae;
struct al;
struct ag;
typedef b::c an;
namespace ai {
template <typename aj> struct ak { typedef aj f; };
template <typename aj> using ar = typename ak<aj>::f;
template <typename> struct am {
  enum { at };
};
template <typename, typename> struct ao {
  enum { at };
};
template <typename> struct ap;
template <typename> struct aq {
  enum { at };
};
} // namespace ai
template <typename> struct ay;
template <typename> class as;
template <typename, int> class ba;
template <typename, int au, int av, int = 0, int = au, int = av> class aw;
template <typename> class be;
template <typename, typename, int> class az;
namespace ai {
template <typename, typename> struct bg;
template <typename aj, int = bg<typename aj::bb, typename aj::bc>::bd>
struct bk;
template <typename, typename> struct bf;
template <typename> struct bm;
template <typename> struct bh;
template <int, typename bi, bool = ao<bi, typename bh<bi>::bj>::at> struct bp {
  typedef bi f;
};
template <typename aj, int bl> struct br {
  typedef typename bp<bl, typename bm<aj>::f>::f f;
};
template <typename, typename, int> struct bn;
template <typename aj, int bo> struct bn<aj, al, bo> {
  typedef aw<typename aj ::bu, aj ::bv, aj ::bq> f;
};
template <typename aj> struct bx {
  typedef typename bn<aj, typename ap<aj>::bs, aj ::bo>::f f;
};
template <typename aj> struct bt { typedef b::n<0, aj, aj> f; };
template <typename aj, int, typename ca = typename bx<aj>::f> struct cb {
  enum { bw };
  typedef b::n<bw, ca, typename bt<aj>::f> f;
};
template <typename cd, typename = typename ap<cd>::bs> struct by {
  typedef be<cd> f;
};
template <typename cd, typename bs> struct bz {
  typedef typename by<cd, bs>::f f;
};
template <typename, typename, int> struct ch;
template <typename ci, int cc> struct ch<ci, ci, cc> { typedef ci bd; };
} // namespace ai
template <typename ck, typename ce, typename = ai::bf<ck, ce>> struct cg;
template <typename aj, typename cm> struct cg<aj, cm> { typedef aj cn; };
namespace ai {
template <typename cj, int> cj cp;
template <typename bu, typename cj, int> void cl(bu *cr, cj cs) { ct(cr, cs); }
typedef __attribute__((altivec(vector__))) double co;
void ct(double *cr, co cs) { *(co *)cr = cs; }
struct cq {
  co q;
};
template <> struct bm<b::af<double>> { typedef cq f; };
template <> struct bh<cq> { typedef cq bj; };
void ct(b::af<double> *cr, cq cs) { ct((double *)cr, cs.q); }
template <typename cw, typename> struct cx {
  template <int cy, typename cj> void cu(cw *a, cj) {
    cl<cw, cj, cy>(a, cp<cj, cy>);
  }
};
} // namespace ai
template <typename cd> class ba<cd, ac> : public ay<cd> {
public:
  typedef ai::ap<cd> bu;
  typedef b::n<ai::ap<cd>::bo, bu, b::n<ai::am<bu>::at, bu, bu>> cv;
  typedef ay<cd> db;
  db::dc;
  cv coeff(an dd, an col) const { return dc().coeff(dd, col); }
};
template <typename cd> class cz : public ba<cd, ai::aq<cd>::at> {
public:
  ai::ap<cd> b;
  enum { da, dg, dh, bv, bq, di = dg, bo };
};
template <typename cd> class be : public cz<cd> {
public:
  typedef typename ai::ap<cd>::bu bu;
  typedef cz<cd> db;
  db::dc;
  template <typename de> cd &operator+=(const be<de> &);
  template <typename de> az<cd, de, ad> df(de);
};
template <typename cd> struct ay {
  cd &dc() { return *static_cast<cd *>(this); }
  cd dc() const;
};
template <typename, typename, int, typename> class dl;
namespace ai {
template <typename bb, typename bc, int dm> struct ap<az<bb, bc, dm>> {
  typedef bb dj;
  typedef bc r;
  typedef ap<dj> s;
  typedef ap<r> t;
  typedef typename cg<typename dj ::bu, typename r ::bu>::cn bu;
  typedef typename ch<typename s::cf, typename t::cf, bg<bb, bc>::bd>::bd cf;
  enum { bo };
};
} // namespace ai
template <typename dk, typename Rhs_, int dm>
class az : public dl<dk, Rhs_, dm,
                     ai::ch<ai::ap<dk>, ai::ap<Rhs_>, ai::bg<dk, Rhs_>::bd>> {
public:
  typedef dk bb;
  typedef Rhs_ bc;
  typedef typename ai::bt<bb>::f LhsNested;
  typedef typename ai::bt<bc>::f dn;
  typedef ai::ar<LhsNested> u;
  typedef ai::ar<dn> RhsNestedCleaned;
  u lhs();
  RhsNestedCleaned rhs();
};
template <typename bb, typename bc, int dm, typename>
class dl : public ai::bz<az<bb, bc, dm>, al>::f {};
namespace ai {
template <typename> struct v { typedef ag w; };
template <typename aj> struct evaluator_traits_base {
  typedef typename v<typename ap<aj>::cf>::w w;
};
template <typename aj> struct ax : evaluator_traits_base<aj> {};
template <typename> struct y { static const bool at = false; };
template <typename bu, int z> class plainobjectbase_evaluator_data {
public:
  plainobjectbase_evaluator_data(bu *ptr, an) : data(ptr) {}
  an outerStride() { return z; }
  bu *data;
};
template <typename cd> struct evaluator {
  typedef cd PlainObjectType;
  typedef typename PlainObjectType::bu bu;
  enum { IsVectorAtCompileTime };
  enum { OuterStrideAtCompileTime };
  evaluator(PlainObjectType &m) : m_d(m.data(), IsVectorAtCompileTime) {}
  bu &coeffRef(an, an) { return m_d.data[m_d.outerStride()]; }
  plainobjectbase_evaluator_data<bu, OuterStrideAtCompileTime> m_d;
};
template <typename bu, int Rows, int Cols, int Options, int MaxRows,
          int MaxCols>
struct evaluator<aw<bu, Rows, Cols, Options, MaxRows, MaxCols>>
    : evaluator<as<aw<bu, Rows, Cols>>> {
  typedef aw<bu, Rows, Cols> XprType;
  evaluator(XprType &m) : evaluator<as<XprType>>(m) {}
};
template <typename DstEvaluator, typename, typename>
struct copy_using_evaluator_traits {
  typedef typename DstEvaluator::bu cw;
  enum { RestrictedInnerSize };
  typedef typename br<cw, RestrictedInnerSize>::f bi;
};
template <typename Kernel, an, int>
struct copy_using_evaluator_innervec_CompleteUnrolling {
  typedef typename Kernel::bi bi;
  enum { outer, inner, SrcAlignment, DstAlignment };
  static void run(Kernel kernel) {
    kernel.template assignPacketByOuterInner<DstAlignment, SrcAlignment, bi>(
        outer, inner);
  }
};
template <typename Kernel> struct dense_assignment_loop {
  static void run(Kernel kernel) {
    typedef typename Kernel::DstEvaluatorType::XprType DstXprType;
    copy_using_evaluator_innervec_CompleteUnrolling<
        Kernel, 0, DstXprType::dh>::run(kernel);
  }
};
template <typename DstEvaluatorTypeT, typename SrcEvaluatorTypeT,
          typename Functor>
class generic_dense_assignment_kernel {
  typedef typename DstEvaluatorTypeT::XprType DstXprType;

public:
  typedef DstEvaluatorTypeT DstEvaluatorType;
  typedef SrcEvaluatorTypeT SrcEvaluatorType;
  typedef copy_using_evaluator_traits<DstEvaluatorTypeT, SrcEvaluatorTypeT,
                                      Functor>
      AssignmentTraits;
  typedef typename AssignmentTraits::bi bi;
  generic_dense_assignment_kernel(DstEvaluatorType dst, SrcEvaluatorType src,
                                  Functor, DstXprType dstExpr)
      : m_dst(dst), m_src(src), m_dstExpr(dstExpr) {}
  template <int StoreMode, int LoadMode, typename cj> void cu(an dd, an col) {
    m_functor.template cu<StoreMode>(
        &m_dst.coeffRef(dd, col), m_src.template packet<LoadMode, cj>(dd, col));
  }
  template <int StoreMode, int LoadMode, typename cj>
  void assignPacketByOuterInner(an, an) {
    an dd;
    an col;
    cu<StoreMode, LoadMode, cj>(dd, col);
  }
  DstEvaluatorType m_dst;
  SrcEvaluatorType &m_src;
  Functor m_functor;
  DstXprType m_dstExpr;
};
template <typename DstXprType, typename SrcXprType, typename Functor>
void call_dense_assignment_loop(DstXprType &dst, SrcXprType src, Functor func) {
  typedef evaluator<DstXprType> DstEvaluatorType;
  typedef evaluator<SrcXprType> SrcEvaluatorType;
  SrcEvaluatorType srcEvaluator(src);
  DstEvaluatorType dstEvaluator(dst);
  typedef generic_dense_assignment_kernel<DstEvaluatorType, SrcEvaluatorType,
                                          Functor>
      Kernel;
  Kernel kernel(dstEvaluator, srcEvaluator, func, dst);
  dense_assignment_loop<Kernel>::run(kernel);
}
template <typename, typename> struct AssignmentKind;
struct Dense2Dense;
template <> struct AssignmentKind<ag, ag> { typedef Dense2Dense Kind; };
template <typename DstXprType, typename SrcXprType, typename,
          typename = typename AssignmentKind<typename ax<DstXprType>::w,
                                             typename ax<SrcXprType>::w>::Kind,
          typename = void>
struct Assignment;
template <typename Dst, typename Src, typename Func>
void call_assignment(Dst &dst, Src src, Func func,
                     b::ab<!y<Src>::at, void *> = 0) {
  enum { NeedToTranspose };
  typedef b::n<NeedToTranspose, Dst, Dst> ActualDstTypeCleaned;
  typedef b::n<NeedToTranspose, Dst, Dst &> ActualDstType;
  ActualDstType actualDst(dst);
  Assignment<ActualDstTypeCleaned, Src, Func>::run(actualDst, src, func);
}
template <typename DstXprType, typename SrcXprType, typename Functor,
          typename Weak>
struct Assignment<DstXprType, SrcXprType, Functor, Weak> {
  static void run(DstXprType &dst, SrcXprType src, Functor func) {
    call_dense_assignment_loop(dst, src, func);
  }
};
template <typename aj, int bl> struct plain_array { aj array[bl]; };
} // namespace ai
template <typename aj, int bl, int, int av, int> class DenseStorage {
  ai::plain_array<aj, bl> m_data;

public:
  an cols() { return av; }
  aj *data() { return m_data.array; }
};
template <typename cd> class as : public ai::by<cd>::f {
public:
  enum { Options };
  typedef typename ai::by<cd>::f db;
  typedef typename ai::ap<cd>::bu bu;
  DenseStorage<bu, db::di, db::da, db::dg, Options> m_storage;
  an cols() { return m_storage.cols(); }
  bu &coeffRef(an, an colId) { return data()[colId]; }
  bu *data() { return m_storage.data(); }
};
namespace ai {
template <typename Scalar_, int au, int av, int Options_, int MaxRows_,
          int MaxCols_>
struct ap<aw<Scalar_, au, av, Options_, MaxRows_, MaxCols_>> {
  typedef Scalar_ bu;
  typedef ae cf;
  typedef al bs;
  enum { bo };
};
} // namespace ai
template <typename Scalar_, int au, int, int Options_, int, int>
class aw : public as<aw<Scalar_, au, Options_>> {
public:
  template <typename T0, typename T1> aw(T0, T1) {}
};
template <typename cd>
template <typename de>
cd &be<cd>::operator+=(const be<de> &other) {
  call_assignment(dc(), other.dc(), ai::cx<bu, bu>());
  return dc();
}
namespace ai {
template <typename, typename> struct bg {
  enum { bd };
};
template <typename bb, typename bc, int Options>
struct evaluator<az<bb, bc, Options>> : bk<az<bb, bc, Options>> {
  typedef az<bb, bc, Options> XprType;
  typedef bk<XprType> db;
  evaluator(XprType xpr) : db(xpr) {}
};
template <typename bb, typename bc, int cc> struct bk<az<bb, bc, ad>, cc> {
  typedef az<bb, bc, ad> XprType;
  bk(XprType xpr)
      : m_lhs(xpr.lhs()), m_rhs(xpr.rhs()), m_lhsImpl(m_lhs), m_rhsImpl(m_rhs) {
  }
  typedef typename cb<bb, bc::dg>::f LhsNested;
  typedef typename cb<bc, bb::da>::f dn;
  typedef LhsNested u;
  typedef dn RhsNestedCleaned;
  typedef u LhsEtorType;
  typedef RhsNestedCleaned RhsEtorType;
  template <int, typename bi> bi packet(an, an);
  LhsNested m_lhs;
  dn m_rhs;
  LhsEtorType m_lhsImpl;
  RhsEtorType m_rhsImpl;
};
} // namespace ai
} // namespace
namespace Eigen {
template <typename Type1, typename Type2> bool verifyIsApprox(Type1, Type2);
}
using namespace Eigen;
template <typename TC, typename TA, typename TB> TC ref_prod(TC C, TA, TB B) {
  for (an i; i;)
    for (an j = 0; j < C.cols(); ++j)
      for (an k; k;)
        C.coeffRef(i, j) += B.coeff(k, j);
  return C;
}
template <typename aj, int Rows, int Cols, int Depth, int OC, int OA, int OB>
b::ab<!0, void> test_lazy_single(int rows, int cols, int depth) {
  aw<aj, Depth, OA> ci(rows, depth);
  aw<aj, Cols, OB> B(depth, cols);
  aw<aj, Rows, OC> C(rows, cols);
  aw D(C);
  verifyIsApprox(C += ci.df(B), ref_prod(D, ci, B));
}
template <typename aj, int Rows, int Cols, int Depth>
void test_lazy_all_layout(int rows = Rows, int cols = Cols, int depth = Depth) {
  test_lazy_single<aj, Rows, Cols, Depth, p, p, p>(rows, cols, depth);
}
template <typename aj> void test_lazy_l2() {
  test_lazy_all_layout<aj, 1, 4, 2>();
}
void fn1() { test_lazy_l2<b::af<double>>(); }
