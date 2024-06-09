namespace {
template <int V> struct J { static constexpr int value = V; };
template <bool V> using K = J<V>;
using M = K<true>;
template <int> struct L { template <typename _Tp, typename> using type = _Tp; };
template <bool _Cond, typename _If, typename _Else> using N = typename L<_Cond>::type<_If, _Else>;
M k;
template <typename _Tp> struct O { using type = _Tp; };
template <typename _Up>
struct P : N<M ::value, O<_Up>, _Up> {};
template <typename _Tp> struct Q { using type = typename P<_Tp>::type; };
}
namespace R {
struct H;
enum G {};
template <typename> class S;
struct T { using U = bool (*) (H &, const H &, G); U F; };
template <typename, typename> class B;
template <typename _R, typename _F, typename... _A>
struct B<_R(_A...), _F> {
  static bool F(H &, const H &, G) { return false; }
  __attribute__((noipa)) static _R bar(const H &) {}
};
template <typename _R, typename... _A>
struct S<_R(_A...)> : T {
  template <typename _F> using AH = B<_R(), _F>;
  template <typename _F> S(_F) {
    using AG = AH<_F>;
    barr = AG::bar;
    F = AG::F;
  }
  using AF = _R (*)(const H &);
  AF barr;
};
template <typename> class I;
template <typename _F, typename... _B>
struct I<_F(_B...)> {};
template <typename> using W = decltype(k);
template <int, typename _F, typename... _B> struct V {
  typedef I<typename Q<_F>::type(typename Q<_B>::type...)> type;
};
template <typename _F, typename... _B>
__attribute__((noipa)) typename V<W<_F>::value, _F, _B...>::type
baz(_F, _B...) { return typename V<W<_F>::value, _F, _B...>::type (); }
template <typename _Tp> struct AJ {
  template <typename _Up> struct _Ptr { using type = _Up *; };
  using AI = typename _Ptr<_Tp>::type;
};
template <typename _Tp> struct Y {
  using AI = typename AJ<_Tp>::AI;
  AI operator->();
};
}
extern int z;
namespace N1 {
namespace N2 {
namespace N3 {
enum Z { Z1, Z2 };
template <int> struct X {
  template <typename _F>
  __attribute__((noipa)) void boo(long long, long long, long long, _F &) {}
};
struct AC {
  AC(int);
  void m1(R::S<void()>);
};
template <typename>
__attribute__((noipa)) void garply(void *, long long, long long, long long) {}
template <>
template <typename _F>
void X<Z2>::boo(long long, long long x, long long y, _F &fi) {
  AC pool(z);
  for (;;) {
    auto job = R::baz(garply<_F>, &fi, y, y, x);
    pool.m1(job);
  }
}
struct AB {
  static AB &bleh();
  template <typename _F>
  void boo(long first, long x, long y, _F fi) {
    switch (ab1) {
    case Z1:
      ab2->boo(first, x, y, fi);
    case Z2:
      ab3->boo(first, x, y, fi);
    }
  }
  Z ab1;
  R::Y<X<Z1>> ab2;
  R::Y<X<Z2>> ab3;
};
template <typename, bool> struct C;
template <typename _F> struct C<_F, false> {
  __attribute__((noipa)) C(_F) {}
  void boo(long first, long x, long y) {
    auto u = AB::bleh();
    u.boo(first, x, y, *this);
  }
};
template <typename _F> struct AA { typedef C<_F, 0> type; };
}
}
}
struct AD {
  template <typename _F>
  static void boo(long first, long x, long y, _F f) {
    typename N1::N2::N3::AA<_F>::type fi(f);
    fi.boo(first, x, y);
  }
  template <typename _F>
  static void boo(long first, long x, _F f) {
    boo(first, x, 0, f);
  }
};
template <typename> struct A {
  void foo(long long, long long);
  int *c;
};
namespace {
template <typename> struct D { __attribute__((noipa)) D(int *) {} };
}
template <typename T>
void A<T>::foo(long long x, long long y)
{
  int e;
  D<T> d(&e);
  AD::boo(0, y, d);
  long p;
  for (p = 0; p < x; p++)
    c[p] = c[p - 1];
}
