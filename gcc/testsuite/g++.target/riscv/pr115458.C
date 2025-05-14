/* { dg-do compile } */
/* { dg-options "-march=rv64gcv1p0 -mabi=lp64d -misa-spec=20191213 -mtls-dialect=trad -march=rv64imafdc_v1p0_zmmul_zca_zcd_zve32f_zve32x_zve64d_zve64f_zve64x_zvl128b_zvl32b_zvl64b -O2 -std=c++17 -fno-exceptions -w" } */

typedef signed char int8_t;
typedef unsigned char uint8_t;
void Abort(...);
template <bool> struct EnableIfT;
template <> struct EnableIfT<true> {
  using type = void;
};
template <bool Condition> using EnableIf = typename EnableIfT<Condition>::type;
template <typename> using MakeUnsigned = unsigned char;
template <typename> using MakeSigned = signed char;
template <int __v> struct integral_constant {
  static constexpr int value = __v;
};
template <bool __v> using __bool_constant = integral_constant<__v>;
template <bool, typename _Tp> using __enable_if_t = _Tp;
char *TargetName();
template <typename _Tp> struct __uniq_ptr_impl {
  template <typename _Up> struct _Ptr {
    using type = _Up *;
  };
  using pointer = typename _Ptr<_Tp>::type;
};
template <typename _Tp, typename = _Tp> class unique_ptr;
template <typename _Tp, typename _Dp> struct unique_ptr<_Tp[], _Dp> {
  template <typename _Up, typename _Del>
  unique_ptr(_Up, __enable_if_t<__bool_constant<false>::value, _Del>);
  typename __uniq_ptr_impl<_Tp>::pointer get();
  operator bool();
};
using AllocPtr = void *;
using FreePtr = void();
template <typename T> T AllocateAlignedItems(int, AllocPtr, void *);
struct AlignedFreer {
  AlignedFreer(FreePtr, void *);
};
template <typename T> using AlignedFreeUniquePtr = unique_ptr<T>;
AllocPtr AllocateAligned_alloc;
template <typename T>
AlignedFreeUniquePtr<T[]> AllocateAligned(int items, void *opaque) {
  FreePtr free;
  return AlignedFreeUniquePtr<T[]>(
      AllocateAlignedItems<T>(items, AllocateAligned_alloc, opaque),
      AlignedFreer(free, opaque));
}
template <typename T> AlignedFreeUniquePtr<T[]> AllocateAligned(int items) {
  return AllocateAligned<T>(items, nullptr);
}
template <typename> void MakeTypeInfo();
void AssertArrayEqual(void *, void *, char *, int);
#pragma riscv intrinsic "vector"
template <typename Lane, int, int kPow2> struct Simd {
  using T = Lane;
  constexpr int Pow2() { return kPow2; }
  template <typename> static constexpr int RebindPow2() { return kPow2; }
  template <typename NewT> using Rebind = Simd<NewT, 0, RebindPow2<NewT>()>;
};
template <typename T, int kPow2> struct ClampNAndPow2 {
  using type = Simd<T, 6, kPow2>;
};
template <typename T, int kPow2> struct ScalableTagChecker {
  using type = typename ClampNAndPow2<T, kPow2>::type;
};
template <typename T, int kPow2>
using ScalableTag = typename ScalableTagChecker<T, kPow2>::type;
template <class D> using TFromD = typename D::T;
template <class T, class D> using Rebind = typename D::Rebind<T>;
template <class D> using RebindToSigned = Rebind<MakeSigned<D>, D>;
template <class D> using RebindToUnsigned = Rebind<MakeUnsigned<D>, D>;
template <class> struct DFromV_t;
template <class V> using DFromV = typename DFromV_t<V>::type;
template <> struct DFromV_t<vint8mf8_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, -3>;
};
template <> struct DFromV_t<vint8mf4_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, -2>;
};
template <> struct DFromV_t<vint8mf2_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, -1>;
};
template <> struct DFromV_t<vint8m1_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, 0>;
};
template <> struct DFromV_t<vint8m2_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, 1>;
};
template <> struct DFromV_t<vint8m4_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, 2>;
};
template <> struct DFromV_t<vint8m8_t> {
  using Lane = int8_t;
  using type = ScalableTag<Lane, 3>;
};
template <int N> int Lanes(Simd<int8_t, N, -3>);
template <int N> int Lanes(Simd<int8_t, N, -2>);
template <int N> int Lanes(Simd<int8_t, N, -1>);
template <int N> int Lanes(Simd<int8_t, N, 0>);
template <int N> int Lanes(Simd<int8_t, N, 1>);
template <int N> int Lanes(Simd<int8_t, N, 2>);
template <int N> int Lanes(Simd<int8_t, N, 3>);
template <int N> vuint8mf8_t Set(Simd<uint8_t, N, -3>, uint8_t);
template <int N> vuint8mf4_t Set(Simd<uint8_t, N, -2>, uint8_t);
template <int N> vuint8mf2_t Set(Simd<uint8_t, N, -1>, uint8_t);
template <int N> vuint8m1_t Set(Simd<uint8_t, N, 0>, uint8_t);
template <int N> vuint8m2_t Set(Simd<uint8_t, N, 1>, uint8_t);
template <int N> vuint8m4_t Set(Simd<uint8_t, N, 2>, uint8_t);
template <int N> vuint8m8_t Set(Simd<uint8_t, N, 3>, uint8_t arg) {
  return __riscv_vmv_v_x_u8m8(arg, 0);
}
template <int N> vint8mf8_t Set(Simd<int8_t, N, -3>, int8_t);
template <int N> vint8mf4_t Set(Simd<int8_t, N, -2>, int8_t);
template <int N> vint8mf2_t Set(Simd<int8_t, N, -1>, int8_t);
template <int N> vint8m1_t Set(Simd<int8_t, N, 0>, int8_t);
template <int N> vint8m2_t Set(Simd<int8_t, N, 1>, int8_t);
template <int N> vint8m4_t Set(Simd<int8_t, N, 2>, int8_t);
template <int N> vint8m8_t Set(Simd<int8_t, N, 3>, int8_t);
template <class D> using VFromD = decltype(Set(D(), TFromD<D>()));
template <class D> VFromD<D> Zero(D d) {
  RebindToUnsigned<decltype(d)> du;
  return BitCast(d, Set(du, 0));
}
template <typename T, int N>
vuint8mf8_t BitCastToByte(Simd<T, N, -3>, vuint8mf8_t);
template <typename T, int N>
vuint8mf4_t BitCastToByte(Simd<T, N, -2>, vuint8mf4_t);
template <typename T, int N>
vuint8mf2_t BitCastToByte(Simd<T, N, -1>, vuint8mf2_t);
template <typename T, int N>
vuint8m1_t BitCastToByte(Simd<T, N, 0>, vuint8m1_t);
template <typename T, int N>
vuint8m2_t BitCastToByte(Simd<T, N, 1>, vuint8m2_t);
template <typename T, int N>
vuint8m4_t BitCastToByte(Simd<T, N, 2>, vuint8m4_t);
template <typename T, int N>
vuint8m8_t BitCastToByte(Simd<T, N, 3>, vuint8m8_t v) {
  return v;
}
template <typename T, int N>
vuint8mf8_t BitCastToByte(Simd<T, N, -3>, vint8mf8_t);
template <int N> vint8mf8_t BitCastFromByte(Simd<int8_t, N, -3>, vuint8mf8_t);
template <typename T, int N>
vuint8mf4_t BitCastToByte(Simd<T, N, -2>, vint8mf4_t);
template <int N> vint8mf4_t BitCastFromByte(Simd<int8_t, N, -2>, vuint8mf4_t);
template <typename T, int N>
vuint8mf2_t BitCastToByte(Simd<T, N, -1>, vint8mf2_t);
template <int N> vint8mf2_t BitCastFromByte(Simd<int8_t, N, -1>, vuint8mf2_t);
template <typename T, int N> vuint8m1_t BitCastToByte(Simd<T, N, 0>, vint8m1_t);
template <int N> vint8m1_t BitCastFromByte(Simd<int8_t, N, 0>, vuint8m1_t);
template <typename T, int N> vuint8m2_t BitCastToByte(Simd<T, N, 1>, vint8m2_t);
template <int N> vint8m2_t BitCastFromByte(Simd<int8_t, N, 1>, vuint8m2_t);
template <typename T, int N> vuint8m4_t BitCastToByte(Simd<T, N, 2>, vint8m4_t);
template <int N> vint8m4_t BitCastFromByte(Simd<int8_t, N, 2>, vuint8m4_t);
template <typename T, int N> vuint8m8_t BitCastToByte(Simd<T, N, 3>, vint8m8_t);
template <int N> vint8m8_t BitCastFromByte(Simd<int8_t, N, 3>, vuint8m8_t v) {
  return __riscv_vreinterpret_v_u8m8_i8m8(v);
}
template <class D, class FromV> VFromD<D> BitCast(D d, FromV v) {
  return BitCastFromByte(d, BitCastToByte(d, v));
}
vint8mf8_t And(vint8mf8_t, vint8mf8_t);
vint8mf4_t And(vint8mf4_t, vint8mf4_t);
vint8mf2_t And(vint8mf2_t, vint8mf2_t);
vint8m1_t And(vint8m1_t, vint8m1_t);
vint8m2_t And(vint8m2_t, vint8m2_t);
vint8m4_t And(vint8m4_t, vint8m4_t);
vint8m8_t And(vint8m8_t, vint8m8_t);
vint8mf8_t Xor(vint8mf8_t, vint8mf8_t);
vint8mf4_t Xor(vint8mf4_t, vint8mf4_t);
vint8mf2_t Xor(vint8mf2_t, vint8mf2_t);
vint8m1_t Xor(vint8m1_t, vint8m1_t);
vint8m2_t Xor(vint8m2_t, vint8m2_t);
vint8m4_t Xor(vint8m4_t, vint8m4_t);
vint8m8_t Xor(vint8m8_t, vint8m8_t);
template <class V> V AndNot(V);
template <class V> V Xor3(V);
template <class V> V Neg(V);
template <int> vuint8mf8_t ShiftLeft(vuint8mf8_t);
template <int> vuint8mf4_t ShiftLeft(vuint8mf4_t);
template <int> vuint8mf2_t ShiftLeft(vuint8mf2_t);
template <int> vuint8m1_t ShiftLeft(vuint8m1_t);
template <int> vuint8m2_t ShiftLeft(vuint8m2_t);
template <int> vuint8m4_t ShiftLeft(vuint8m4_t);
template <int> vuint8m8_t ShiftLeft(vuint8m8_t);
vint8mf8_t MaskedSubOr(vint8mf8_t, vbool64_t, vint8mf8_t, vint8mf8_t);
vint8mf4_t MaskedSubOr(vint8mf4_t, vbool32_t, vint8mf4_t, vint8mf4_t);
vint8mf2_t MaskedSubOr(vint8mf2_t, vbool16_t, vint8mf2_t, vint8mf2_t);
vint8m1_t MaskedSubOr(vint8m1_t, vbool8_t, vint8m1_t, vint8m1_t);
vint8m2_t MaskedSubOr(vint8m2_t, vbool4_t, vint8m2_t, vint8m2_t);
vint8m4_t MaskedSubOr(vint8m4_t, vbool2_t, vint8m4_t, vint8m4_t);
vint8m8_t MaskedSubOr(vint8m8_t no, vbool1_t m, vint8m8_t a, vint8m8_t b) {
  return __riscv_vsub_vv_i8m8_mu(m, no, a, b, 0);
}
vbool64_t Lt(vint8mf8_t, vint8mf8_t);
vbool32_t Lt(vint8mf4_t, vint8mf4_t);
vbool16_t Lt(vint8mf2_t, vint8mf2_t);
vbool8_t Lt(vint8m1_t, vint8m1_t);
vbool4_t Lt(vint8m2_t, vint8m2_t);
vbool2_t Lt(vint8m4_t, vint8m4_t);
vbool1_t Lt(vint8m8_t a, vint8m8_t b) {
  return __riscv_vmslt_vv_i8m8_b1(a, b, 0);
}
template <class V> V BroadcastSignBit(V);
template <class V> V IfNegativeThenElse(V);
template <int N> void Store(vint8mf8_t, Simd<int8_t, N, -3>, int8_t *);
template <int N> void Store(vint8mf4_t, Simd<int8_t, N, -2>, int8_t *);
template <int N> void Store(vint8mf2_t, Simd<int8_t, N, -1>, int8_t *);
template <int N> void Store(vint8m1_t, Simd<int8_t, N, 0>, int8_t *);
template <int N> void Store(vint8m2_t, Simd<int8_t, N, 1>, int8_t *);
template <int N> void Store(vint8m4_t, Simd<int8_t, N, 2>, int8_t *);
template <int N> void Store(vint8m8_t, Simd<int8_t, N, 3>, int8_t *);
template <class D, class V, EnableIf<D().Pow2() <= 2> * = nullptr>
V InterleaveUpperBlocks(D, V, V) {}
template <class D, class V, EnableIf<(D().Pow2() > 2)> * = nullptr>
V InterleaveUpperBlocks(D, V, V);
template <typename T, int N, int kPow2>
constexpr bool IsGE128(Simd<T, N, kPow2>) {
  return kPow2 >= 0;
}
template <class D, class V, EnableIf<IsGE128(D())> * = nullptr>
V InterleaveLower(D, V, V);
template <class D, class V, EnableIf<!IsGE128(D())> * = nullptr>
V InterleaveLower(D, V, V);
template <class D, class V, EnableIf<IsGE128(D())> * = nullptr>
V InterleaveUpper(D d, V a, V b) {
  return InterleaveUpperBlocks(d, a, b);
}
template <class D, class V, EnableIf<!IsGE128(D())> * = nullptr>
V InterleaveUpper(D, V, V);
template <class D, typename T2> VFromD<D> Iota(D, T2);
template <class D> using Vec = decltype(Zero(D()));
template <class D> Vec<D> SignBit(D);
template <class V> V IfNegativeThenElseZero(V);
template <class V> V IfNegativeThenZeroElse(V);
template <class V> V BitwiseIfThenElse(V, V, V);
template <class V> inline V IfNegativeThenNegOrUndefIfZero(V mask, V v) {
  auto zero = Zero(DFromV<V>());
  return MaskedSubOr(v, Lt(mask, zero), zero, v);
}
template <class D> Vec<D> PositiveIota(D);
int AssertVecEqual_line;
template <class D, typename T = TFromD<D>>
inline void AssertVecEqual(D d, Vec<D> expected, Vec<D> actual, char *) {
  int N = Lanes(d);
  auto expected_lanes = AllocateAligned<T>(N),
       actual_lanes = AllocateAligned<T>(N);
  if (expected_lanes && actual_lanes)
    Abort("", "");
  Store(expected, d, expected_lanes.get());
  Store(actual, d, actual_lanes.get());
  MakeTypeInfo<T>();
  char *target_name = TargetName();
  AssertArrayEqual(expected_lanes.get(), actual_lanes.get(), target_name,
                   AssertVecEqual_line);
}
template <typename> constexpr int MinPow2() { return sizeof(int) ? -3 : 0; }
template <typename T, int kPow2, int kMaxPow2, int, class Test>
struct ForeachPow2 {
  static void Do(int min_lanes) {
    ScalableTag<T, kPow2> d;
    Lanes(d);
    Test()(T(), d);
    ForeachPow2<T, kPow2 + 1, kMaxPow2, kPow2 + 1 <= kMaxPow2, Test>::Do(
        min_lanes);
  }
};
template <typename T, int kPow2, int kMaxPow2, class Test>
struct ForeachPow2<T, kPow2, kMaxPow2, false, Test> {
  static void Do(int);
};
template <typename T, int kAddMin, int kSubMax, class Test>
using ForeachPow2Trim =
    ForeachPow2<T, MinPow2<T>(), 3, kAddMin <= kSubMax, Test>;
template <class Test, int kPow2> struct ForExtendableVectors {
  template <typename T> void operator()(T) {
    ForeachPow2Trim<T, 0, kPow2, Test>::Do(1);
  }
};
template <class Test> struct ForPartialVectors {
  template <typename T> void operator()(T t) {
    ForExtendableVectors<Test, 0>()(t);
  }
};
template <class Func> void ForSignedTypes(Func func) { func(int8_t()); }
struct TestIfNegative {
  template <class T, class D> void operator()(T, D d) {
    auto vp = Iota(d, 1), vsignbit = SignBit(d);
    RebindToSigned<decltype(d)> di;
    RebindToUnsigned<decltype(d)> du;
    BitCast(d, ShiftLeft<sizeof(TFromD<decltype(d)>)>(Iota(du, 1)));
    auto m1 = Xor3(BitCast(d, Set(du, {})));
    auto x1 = Xor(vp, BitCast(d, Set(d, {})));
    auto x2 = Xor(vp, BitCast(d, Set(d, {})));
    Xor(m1, vsignbit);
    auto m1_s = BitCast(d, BroadcastSignBit(BitCast(di, m1)));
    auto expected_2 = BitwiseIfThenElse(m1_s, x2, x1);
    AssertVecEqual(d, expected_2, IfNegativeThenElse(x2), "");
    auto expected_3 = And(m1_s, x1);
    auto expected_4 = AndNot(x2);
    AssertVecEqual(d, expected_3, IfNegativeThenElseZero(x1), "");
    AssertVecEqual(d, expected_3, IfNegativeThenZeroElse(x1), "");
    AssertVecEqual(d, expected_4, IfNegativeThenZeroElse(x2), "");
    AssertVecEqual(d, expected_4, IfNegativeThenElseZero(x2), "");
  }
};
void TestAllIfNegative() {
  ForSignedTypes(ForPartialVectors<TestIfNegative>());
}
template <class D>
void TestMoreThan1LaneIfNegativeThenNegOrUndefIfZero(D d, Vec<D> v1) {
  Vec<D> v2, v3 = InterleaveLower(d, v1, v1), v5 = InterleaveLower(d, v1, v2);
  if (Lanes(d) < 2)
    return;
  Vec<D> v4 = InterleaveUpper(d, v1, v1);
  Vec<D> v6 = InterleaveUpper(d, v1, v2);
  Vec<D> v7 = InterleaveLower(d, v2, v1);
  Vec<D> v8 = InterleaveUpper(d, v2, v1);
  AssertVecEqual(d, v3, IfNegativeThenNegOrUndefIfZero(v3, v3), "");
  AssertVecEqual(d, v4, IfNegativeThenNegOrUndefIfZero(v4, v4), "");
  AssertVecEqual(d, v4, IfNegativeThenNegOrUndefIfZero(v8, v8), "");
  AssertVecEqual(d, v6, IfNegativeThenNegOrUndefIfZero(v4, v6), "");
  AssertVecEqual(d, v7, IfNegativeThenNegOrUndefIfZero(v3, v7), "");
  AssertVecEqual(d, v8, IfNegativeThenNegOrUndefIfZero(v4, v8), "");
  Vec<D> zero = Zero(d);
  AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v3, zero), "");
  AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v4, zero), "");
  AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v5, zero), "");
  AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v6, zero), "");
  AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v7, zero), "");
  AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v8, zero), "");
}
struct TestIfNegativeThenNegOrUndefIfZero {
  template <typename T, class D> void operator()(T, D d) {
    auto v1 = PositiveIota(d), v2 = Neg(v1), zero = Zero(d), vmin = Set(d, 0),
         vmax = Set(d, 0);
    AssertVecEqual(d, v2, IfNegativeThenNegOrUndefIfZero(v1, v2), "");
    AssertVecEqual(d, v2, IfNegativeThenNegOrUndefIfZero(v2, v1), "");
    AssertVecEqual(d, v1, IfNegativeThenNegOrUndefIfZero(v2, v2), "");
    AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(zero, zero), "");
    AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v1, zero), "");
    AssertVecEqual(d, zero, IfNegativeThenNegOrUndefIfZero(v2, zero), "");
    AssertVecEqual(d, v1, IfNegativeThenNegOrUndefIfZero(vmin, v2), "");
    AssertVecEqual(d, v1, IfNegativeThenNegOrUndefIfZero(vmax, v1), "");
    AssertVecEqual(d, v2, IfNegativeThenNegOrUndefIfZero(vmax, v2), "");
    TestMoreThan1LaneIfNegativeThenNegOrUndefIfZero(d, v1);
  }
};
void TestAllIfNegativeThenNegOrUndefIfZero() {
  ForSignedTypes(ForPartialVectors<TestIfNegativeThenNegOrUndefIfZero>());
}
