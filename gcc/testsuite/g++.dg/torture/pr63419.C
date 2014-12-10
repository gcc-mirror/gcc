// { dg-do compile }
// { dg-additional-options "-Wno-psabi" }
// Ignore warning on some powerpc-linux configurations.
// { dg-prune-output "non-standard ABI extension" }

typedef float __m128 __attribute__ ((__vector_size__ (16)));
const int a = 0;
enum
{
  ReadOnlyAccessors
};
template <typename> struct traits;
struct A
{
  enum
  {
    value = 1
  };
};
template <typename> struct EigenBase;
template <typename> class J;
template <typename, int = A::value> class DenseCoeffsBase;
template <typename, int, int, int = 0, int = 0, int = 0> class K;
template <typename> class N;
template <typename, typename> class CwiseUnaryOp;
template <typename> class L;
template <typename> class P;
template <typename> struct B;
template <typename> struct C;
template <typename Derived> struct dense_xpr_base
{
  typedef N<Derived> type;
};
template <typename Packet> void padd (Packet);
template <> struct C<float>
{
  typedef __m128 type;
};
struct D
{
  void
  packetOp (C<float>::type)
  {
    __m128 b = { m_other };
    padd (b);
  }
  float m_other;
};
template <typename Derived>
class DenseCoeffsBase<Derived, ReadOnlyAccessors> : public EigenBase<Derived>
{
public:
  typedef typename C<typename traits<Derived>::Scalar>::type PacketScalar;
};
template <typename Derived>
class DenseCoeffsBase<Derived>
    : public DenseCoeffsBase<Derived, ReadOnlyAccessors>
{
public:
  template <typename OtherDerived, int, int LoadMode>
  void
  copyPacket (typename traits<Derived>::Index, J<OtherDerived> p2)
  {
    p2.derived ().template packet<LoadMode> (0);
  }
};
template <typename Derived> class J : public DenseCoeffsBase<Derived>
{
public:
  using DenseCoeffsBase<Derived>::derived;
  template <typename OtherDerived>
  Derived &lazyAssign (const J<OtherDerived> &);
};
template <typename Derived> class N : public J<Derived>
{
public:
  template <typename OtherDerived>
  typename B<OtherDerived>::Type operator*(N<OtherDerived>);
  L<Derived> array ();
};
template <typename Derived> struct EigenBase
{
  Derived
  derived () const
  {
    return *static_cast<const Derived *> (this);
  }
};
template <typename Derived1, typename Derived2> struct F
{
  static void
  run (Derived1 p1, Derived2 p2)
  {
    enum
    {
      srcAlignment
    };
    for (;;)
      p1.template copyPacket<Derived2, 0, srcAlignment> (0, p2);
  }
};
template <typename Derived>
template <typename OtherDerived>
Derived &
J<Derived>::lazyAssign (const J<OtherDerived> &p1)
{
  F<Derived, OtherDerived>::run (derived (), p1.derived ());
}
template <typename Derived, typename OtherDerived> struct G
{
  static Derived
  run (Derived p1, OtherDerived p2)
  {
    p1.lazyAssign (p2);
  }
};
class H
{
public:
  H (int, int, int);
};
template <typename Derived> class M : public dense_xpr_base<Derived>::type
{
public:
  typedef typename traits<Derived>::Index Index;
  H m_storage;
  M (Index, Index, Index) : m_storage (0, 0, 0) {}
  template <typename OtherDerived>
  void
  _set_noalias (J<OtherDerived> p1)
  {
    G<Derived, OtherDerived>::run (this->derived (), p1.derived ());
  }
};
template <typename _Scalar, int _Rows, int _Cols, int _Options, int _MaxRows,
          int _MaxCols>
struct traits<K<_Scalar, _Rows, _Cols, _Options, _MaxRows, _MaxCols> >
{
  typedef _Scalar Scalar;
  typedef int StorageKind;
  typedef int Index;
};
template <typename, int _Rows, int _Cols, int, int, int>
class K : public M<K<float, _Rows, _Cols> >
{
public:
  typedef M<K> Base;
  typedef K Nested;
  template <typename T0, typename T1> K (T0, T1);
  template <typename OtherDerived> K (N<OtherDerived> p1) : Base (0, 0, 0)
  {
    Base::_set_noalias (p1);
  }
};
template <typename UnaryOp, typename XprType>
struct traits<CwiseUnaryOp<UnaryOp, XprType> > : traits<XprType>
{
};
template <typename, typename, typename> class I;
template <typename, typename XprType>
class CwiseUnaryOp
    : public I<D, XprType, typename traits<XprType>::StorageKind>
{
public:
  D
  functor ()
  {
    return m_functor;
  }
  typename XprType::Nested nestedExpression ();
  D m_functor;
};
template <typename UnaryOp, typename XprType>
class I<UnaryOp, XprType, int>
    : public dense_xpr_base<CwiseUnaryOp<UnaryOp, XprType> >::type
{
public:
  typedef CwiseUnaryOp<UnaryOp, XprType> Derived;
  typedef typename dense_xpr_base<CwiseUnaryOp<UnaryOp, XprType> >::type Base;
  typedef Derived Nested;
  using Base::derived;
  template <int LoadMode> void packet (typename traits<Derived>::Index)
  {
    derived ().functor ().packetOp (
        derived ().nestedExpression ().template packet<LoadMode> (0));
  }
};
template <typename> struct B
{
  typedef P<CwiseUnaryOp<D, L<K<float, 0, 1> > > > Type;
};
template <typename Derived> class O : public J<Derived>
{
public:
  P<Derived> matrix ();
};
template <typename ExpressionType>
struct traits<L<ExpressionType> > : traits<typename ExpressionType::Nested>
{
};
template <typename ExpressionType> class L : public O<L<ExpressionType> >
{
public:
  typedef L Nested;
  template <int>
  typename O<L>::PacketScalar packet (typename traits<L>::Index);
};
template <typename ExpressionType>
struct traits<P<ExpressionType> > : traits<typename ExpressionType::Nested>
{
};
template <typename ExpressionType> class P : public N<P<ExpressionType> >
{
public:
  N<P> Base;
  template <int LoadMode> void packet (typename traits<P>::Index)
  {
    m_expression.template packet<LoadMode> (0);
  }
  ExpressionType m_expression;
};
int
main ()
{
  K<float, 0, a> m (0, 0);
  K<float, 0, 1> c (0, 0);
  c = m.array ().matrix () * m;
}
