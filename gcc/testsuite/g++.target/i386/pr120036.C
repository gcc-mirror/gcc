/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -std=c++11 -march=sapphirerapids -fPIC" } */

typedef _Float16 Native;
struct float16_t
{
  Native native;
  float16_t ();
  float16_t (Native arg) : native (arg) {}
  operator Native ();
  float16_t
  operator+ (float16_t rhs)
  {
    return native + rhs.native;
  }
  float16_t
  operator* (float16_t)
  {
    return native * native;
  }
};
template <int N> struct Simd
{
  static constexpr int kPrivateLanes = N;
};
template <int N> struct ClampNAndPow2
{
  using type = Simd<N>;
};
template <int kLimit> struct CappedTagChecker
{
  static constexpr int N = sizeof (int) ? kLimit : 0;
  using type = typename ClampNAndPow2<N>::type;
};
template <typename, int kLimit, int>
using CappedTag = typename CappedTagChecker<kLimit>::type;
template <class D>
int
Lanes (D)
{
  return D::kPrivateLanes;
}
template <class D> int Zero (D);
template <class D> using VFromD = decltype (Zero (D ()));
struct Vec512
{
  __attribute__ ((__vector_size__ (16))) _Float16 raw;
};
Vec512 Zero (Simd<2>);
template <class D> void ReduceSum (D, VFromD<D>);
struct Dot
{
  template <int, class D, typename T>
  static T
  Compute (D d, T *pa, int num_elements)
  {
    T *pb;
    int N = Lanes (d), i = 0;
    if (__builtin_expect (num_elements < N, 0))
      {
        T sum0 = 0, sum1 = 0;
        for (; i + 2 <= num_elements; i += 2)
          {
            float16_t __trans_tmp_6 = pa[i] * pb[i],
                      __trans_tmp_5 = sum0 + __trans_tmp_6,
                      __trans_tmp_8 = pa[i + 1] * pb[1],
                      __trans_tmp_7 = sum1 + __trans_tmp_8;
            sum0 = __trans_tmp_5;
            sum1 = __trans_tmp_7;
          }
        float16_t __trans_tmp_9 = sum0 + sum1;
        return __trans_tmp_9;
      }
    decltype (Zero (d)) sum0;
    ReduceSum (d, sum0);
    __builtin_trap ();
  }
};
template <int kMul, class Test, int kPow2> struct ForeachCappedR
{
  static void
  Do (int min_lanes, int max_lanes)
  {
    CappedTag<int, kMul, kPow2> d;
    Test () (int (), d);
    ForeachCappedR<kMul / 2, Test, kPow2>::Do (min_lanes, max_lanes);
  }
};
template <class Test, int kPow2> struct ForeachCappedR<0, Test, kPow2>
{
  static void Do (int, int);
};
struct TestDot
{
  template <class T, class D>
  void
  operator() (T, D d)
  {
    int counts[]{ 1, 3 };
    for (int num : counts)
      {
        float16_t a;
        T __trans_tmp_4 = Dot::Compute<0> (d, &a, num);
      }
  }
};
int DotTest_TestAllDot_TestTestBody_max_lanes;
void
DotTest_TestAllDot_TestTestBody ()
{
  ForeachCappedR<64, TestDot, 0>::Do (
      1, DotTest_TestAllDot_TestTestBody_max_lanes);
}
