// { dg-do compile }

enum { Aligned, RowMajor };
enum { ReadOnlyAccessors };
template <typename> struct K {
  enum { value };
};
template <typename> struct traits;
template <typename T> struct traits<const T> : traits<T> {};
struct A {
  enum { has_write_access, value };
};
template <typename, int n> class array {
public:
  int operator[](unsigned long p1) { return values[p1]; }
  int values[n];
};
template <typename> struct I;
template <typename, int, template <class> class = I> class M;
template <typename, int, int, typename> class J;
template <typename, int> class N;
template <typename, typename> class D;
template <typename, typename, typename, typename> class TensorContractionOp;
template <long, typename> class TensorChippingOp;
class C;
template <typename DenseIndex, int NumDims>
struct K<array<DenseIndex, NumDims>> {
  static const long value = NumDims;
};
template <typename Scalar_, int NumIndices_, int Options_, typename IndexType_>
struct traits<J<Scalar_, NumIndices_, Options_, IndexType_>> {
  typedef IndexType_ Index;
};
template <typename PlainObjectType, int Options_,
          template <class> class MakePointer_>
struct traits<M<PlainObjectType, Options_, MakePointer_>>
    : traits<PlainObjectType> {};
template <typename T> struct B { typedef T type; };
template <typename Derived> class N<Derived, ReadOnlyAccessors> {
public:
  typedef typename traits<Derived>::Index Index;
  D<int, Derived> m_fn1();
  template <typename OtherDerived, typename Dimensions>
  TensorContractionOp<Dimensions, Derived, const OtherDerived, int>
      m_fn2(OtherDerived, Dimensions);
  template <Index> TensorChippingOp<1, Derived> m_fn3(Index);
};
template <typename Derived, int = A::value>
class N : public N<Derived, ReadOnlyAccessors> {
public:
  template <typename DeviceType> C m_fn4(DeviceType);
};
template <typename, typename> struct TensorEvaluator;
template <typename UnaryOp, typename ArgType, typename Device>
struct TensorEvaluator<const D<UnaryOp, ArgType>, Device> {
  TensorEvaluator(D<UnaryOp, ArgType>, Device);
};
template <typename, typename> class D {
public:
  typedef typename B<D>::type Nested;
};
template <typename Indices_, typename LeftArgType_, typename RightArgType_,
          typename OutputKernelType_, typename Device_>
struct traits<
    TensorEvaluator<const TensorContractionOp<Indices_, LeftArgType_,
                                              RightArgType_, OutputKernelType_>,
                    Device_>> {
  typedef Indices_ Indices;
  typedef LeftArgType_ LeftArgType;
  typedef RightArgType_ RightArgType;
  typedef OutputKernelType_ OutputKernelType;
  typedef Device_ Device;
};
template <typename, typename LhsXprType, typename RhsXprType, typename>
class TensorContractionOp {
public:
  typedef typename B<TensorContractionOp>::type Nested;
  typename LhsXprType::Nested m_fn5();
  typename RhsXprType::Nested m_fn6();
};
template <typename Derived> struct TensorContractionEvaluatorBase {
  typedef typename traits<Derived>::LeftArgType LeftArgType;
  typedef typename traits<Derived>::RightArgType RightArgType;
  typedef typename traits<Derived>::Device Device;
  TensorContractionEvaluatorBase(
      TensorContractionOp<typename traits<Derived>::Indices, LeftArgType,
                          RightArgType,
                          typename traits<Derived>::OutputKernelType>
          p1,
      Device p2)
      : m_leftImpl(p1.m_fn6(), p2), m_rightImpl(p1.m_fn5(), p2) {
    long nocontract_idx;
    for (int i;; i++) {
      bool contracting;
      if (contracting) {
        if (nocontract_idx < K<int>::value)
          m_j_size = m_j_strides[nocontract_idx];
        nocontract_idx++;
      }
    }
  }
  array<long, 1> m_j_strides;
  long m_j_size;
  TensorEvaluator<RightArgType, Device> m_leftImpl;
  TensorEvaluator<LeftArgType, Device> m_rightImpl;
};
template <typename Indices, typename LeftArgType, typename RightArgType,
          typename OutputKernelType, typename Device>
struct TensorEvaluator<
    const TensorContractionOp<Indices, LeftArgType, RightArgType,
                              OutputKernelType>,
    Device>
    : TensorContractionEvaluatorBase<TensorEvaluator<
          const TensorContractionOp<Indices, LeftArgType, RightArgType,
                                    OutputKernelType>,
          Device>> {
  typedef TensorEvaluator Self;
  typedef TensorContractionEvaluatorBase<Self> Base;
  TensorEvaluator(
      TensorContractionOp<Indices, LeftArgType, RightArgType, OutputKernelType>
          p1,
      Device p2)
      : Base(p1, p2) {}
};
template <long DimId, typename XprType>
struct traits<TensorChippingOp<DimId, XprType>> : traits<XprType> {};
template <long, typename XprType>
class TensorChippingOp : public N<TensorChippingOp<1, XprType>> {
public:
  typedef typename B<TensorChippingOp>::type Nested;
};
template <long DimId, typename ArgType, typename Device>
struct TensorEvaluator<const TensorChippingOp<DimId, ArgType>, Device> {
  static const int NumInputDims = K<typename ArgType::Dimensions>::value;
  array<long, NumInputDims> m_dimensions;
};
template <long DimId, typename ArgType, typename Device>
struct TensorEvaluator<TensorChippingOp<DimId, ArgType>, Device>
    : TensorEvaluator<const TensorChippingOp<1, ArgType>, Device> {
  TensorEvaluator(TensorChippingOp<DimId, ArgType>, Device);
};
template <typename, typename RhsXprType> class TensorAssignOp {
public:
  TensorAssignOp(TensorChippingOp<0, const M<J<int, 3, 1, int>, 1>>,
                 RhsXprType);
  TensorChippingOp<0, const M<J<int, 3, 1, int>, 1>> m_fn7();
  typename RhsXprType::Nested m_fn8();
};
template <typename LeftArgType, typename RightArgType, typename Device>
struct TensorEvaluator<const TensorAssignOp<LeftArgType, RightArgType>,
                       Device> {
  TensorEvaluator(TensorAssignOp<LeftArgType, RightArgType> p1, Device p2)
      : m_leftImpl(p1.m_fn7(), p2), m_rightImpl(p1.m_fn8(), p2) {}
  TensorEvaluator<LeftArgType, Device> m_leftImpl;
  TensorEvaluator<RightArgType, Device> m_rightImpl;
};
template <typename Expression> class F {
public:
  static void m_fn9(Expression p1) {
    int device;
    TensorEvaluator<Expression, int>(p1, device);
  }
};
class C {
public:
  void
  operator=(TensorContractionOp<array<int, 1>,
                                TensorChippingOp<1, M<J<float, 3, 1, int>, 0>>,
                                const D<int, M<J<float, 3, 1, int>, 0>>, int>
                p1) {
    TensorAssignOp<
        TensorChippingOp<0, const M<J<int, 3, 1, int>, 1>>,
        const TensorContractionOp<
            array<int, 1>, TensorChippingOp<1, M<J<float, 3, 1, int>, 0>>,
            const D<int, M<J<float, 3, 1, int>, 0>>, int>>
        assign(m_expression, p1);
    F<const TensorAssignOp<
        TensorChippingOp<0, const M<J<int, 3, 1, int>, 1>>,
        const TensorContractionOp<
            array<int, 1>, TensorChippingOp<1, M<J<float, 3, 1, int>, 0>>,
            const D<int, M<J<float, 3, 1, int>, 0>>, int>>>::m_fn9(assign);
  }
  TensorChippingOp<0, const M<J<int, 3, 1, int>, 1>> m_expression;
};
template <typename, int NumIndices_, int, typename> class J {
public:
  typedef array<long, NumIndices_> Dimensions;
};
template <typename PlainObjectType, int Options_, template <class> class>
class M : public N<M<PlainObjectType, Options_>> {
public:
  typedef typename PlainObjectType::Dimensions Dimensions;
};
template <int NDIMS> struct TTypes {
  typedef M<J<float, NDIMS, RowMajor, int>, Aligned> ConstTensor;
};
class L {
public:
  template <typename, long NDIMS> typename TTypes<NDIMS>::ConstTensor m_fn10();
};
class H {
public:
  H(int *);
};
class G {
public:
  G(H *(int *));
};
int Run_d;
class O : H {
public:
  int BatchMatMul_context;
  O() : H(&BatchMatMul_context) {
    L out, in_y, in_x;
    auto Tx = in_x.m_fn10<float, 3>(), Ty = in_y.m_fn10<float, 3>(),
         Tz = out.m_fn10<float, 3>(), z = Tz;
    array<int, 1> contract_pairs;
    auto x = Tx.m_fn3<0>(0);
    auto y = Ty.m_fn1();
    z.m_fn4(Run_d) = x.m_fn2(y, contract_pairs);
  }
};
G registrar__body__0__object([](int *) -> H * { O(); return 0; });
