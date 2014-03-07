// PR debug/52260
// { dg-do compile { target c++11 } }
// { dg-options "-gdwarf-4" }

namespace { typedef decltype (nullptr) T1; }
struct B {};
namespace A
{
  template <typename T, T __v>
  struct C { static constexpr T value = __v; };
  typedef C <bool, false> D;
  template <typename>
  struct E : D {};
  template <typename T>
  struct F : C <bool, (E <T>::value)> {};
  template <bool, typename T = void>
  struct G { typedef T t; };
}
template <typename T>
struct H {};
namespace A
{
  template <typename T>
  struct I : H <T> {};
  template <typename ...> struct J {};
  template <typename> struct K;
  struct L
  {
    template <typename B2>
    struct M
    {
      template <typename T> static bool m2 (T) { return false; }
    };
  };
  template <typename, typename> struct N;
  template <typename T, typename B2, typename ... B4>
  struct N <T (B4 ...), B2> : L::M <B2> {};
  template <typename T, typename ... B4>
  struct K <T (B4 ...)> :J <>, L
  {
    typedef T O (B4 ...);
    struct P {};
    template <typename B2> K (B2, typename G <!F <B2>::value, P>::t = P ());
  };
  template <typename T, typename ... B1>
  template <typename B2>
  K <T (B1 ...)>::K (B2 __f, typename G <!F < B2>::value, P>::t)
  {
    typedef N <O, B2> Q;
    Q::m2 (__f);
  };
}
enum R { r1 };
const R r2 = r1;
namespace A
{
  template <typename>
  struct S {};
  template <typename T, typename _Dp = S <T>>
  struct U {};
  template <typename T, R _Lp = r2>
  struct V { T *operator -> (); };
  template <typename T>
  struct W : V <T>
  {
    W (const W &);
    W (T1) {}
    W & operator= (W) {}
  };
  template <typename> struct Z;
  struct AA
  {
    struct AB
    {
      struct AC { void operator () () {} };
    };
    template <typename T> using AD = U <T, AB::AC>;
    struct AE
    {
      typedef AD <AB> AZ;
      virtual ~AE ();
      void o3 (K <AZ ()>, bool = false) {}
      template <typename, typename _Arg> struct AY;
      struct C1 {};
      template <typename T> struct AY <T, C1>
      {
	AZ operator () () { return AZ (); }
        Z <T> _M_Z;
      };
      template <typename T>
      static AY <T, C1> _s1 (B, Z <T> *);
    };
  };
  template <>
  struct Z <void>
  {
    typedef AA::AE AF;
    W <AF> o4;
    void foo (B __p)
    {
      auto _s1 = AF::_s1 (__p, this);
      o4->o3 (_s1);
    }
  };
  template <typename T, typename _Alloc>
  struct AG {};
  template <typename T, typename D1 = A::I <T>>
  struct AH : AG <T, D1>
  {
    void bar (T) { baz (); }
    template <typename ... _Args>
    void baz (_Args && ...);
  };
  template <typename T, typename D1>
  template <typename ... _Args>
  void AH <T, D1>::baz (_Args && ...) {}
  namespace
  {
    typedef A::K <void ()> AI;
    struct AF
    {
      int v2;
      AI v1;
      AF (int, unsigned, AI __t) : v2 (), v1 (__t) {}
    };
    struct D3 : A::AH <AF>
    {
      typedef AF AFT;
      void v3 (AI __t) { bar (AFT (4, v4, __t)); }
      int v4;
    };
  }
}
