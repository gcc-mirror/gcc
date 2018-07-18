// PR c++/84314
// { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-additional-options "-w -std=c++11" }

template <typename a, a b> struct c { static constexpr a d = b; };
template <bool b> using e = c<bool, b>;
template <bool, typename, typename> struct conditional;
template <typename...> struct f;
template <typename g, typename h>
struct f<g, h> : conditional<g::d, g, h>::i {};
template <typename...> struct j;
template <typename g, typename h> struct j<g, h> : conditional<1, h, g>::i {};
template <typename g, typename h, typename k, typename... l>
struct j<g, h, k, l...> : conditional<1, j<h, k>, g>::i {};
struct aa : e<!bool()> {};
template <typename, typename> struct m : c<bool, false> {};
template <typename, typename n> struct o {
  template <typename> static c<bool, true> p(int);
  typedef decltype(p<n>(0)) i;
};
template <typename, typename> struct ab : o<int, int>::i {};
template <typename> struct s { typedef int ad; };
template <bool, typename = void> struct q;
template <typename a> struct q<true, a> { typedef a i; };
template <bool, typename ae, typename> struct conditional { typedef ae i; };
template <typename ae, typename r> struct conditional<false, ae, r> {
  typedef r i;
};
struct B {
  B(int);
};
template <unsigned, typename...> struct af;
template <unsigned ag, typename t, typename... ah>
struct af<ag, t, ah...> : af<1, ah...>, B {
  typedef af<1, ah...> ai;
  ai al(af);
  template <typename... am> af(af<ag, am...> p1) : ai(al(p1)), B(0) {}
};
template <unsigned ag, typename t> struct af<ag, t> {};
template <int, typename... ao> struct ap {
  template <typename... am> static constexpr bool ar() {
    return j<ab<am, ao>...>::d;
  }
};
template <typename... ao> class as : public af<0, ao...> {
  typedef af<0, ao...> ai;

public:
  template <typename...> using au = ap<m<int, int>::d, ao...>;
  template <typename... am,
            typename q<au<>::template ar<am...>(), bool>::i = true>
  as(as<am...> an) : ai(an) {}
};
template <typename... ao> as<typename s<ao>::ad...> ax(ao...);
namespace ay {
class az {};
}
using ay::az;
namespace ay {
template <typename ba> struct C { typedef ba bc; };
}
template <typename> class bd;
template <typename bi, typename n> using bj = f<m<bi, n>, ab<bi, n>>;
template <typename bf, typename... bh> class bd<bf(bh...)> {
  struct F : bj<int, bf> {};
  template <typename bl, typename> using bm = typename q<bl::d>::i;

public:
  template <typename bg, typename = bm<aa, void>, typename = bm<F, void>>
  bd(bg);
  using bn = bf;
  bn bo;
};
template <typename bf, typename... bh>
template <typename bg, typename, typename>
bd<bf(bh...)>::bd(bg) {
  bo;
}
typedef long long(__attribute__((fastcall)) bq)(int *);
struct v : ay::C<as<bq, bq, int>> {
  bc bt() { return ax(nullptr, nullptr, az()); }
};
class w {
public:
  int *cc();
};
class x : w {
  void ce();
};
namespace u {
class cf {
public:
  static cf cg(int, int *, int, az, bd<long long(int *)>);
};
}
void x::ce() {
  auto bu = 0;
  u::cf::cg(bu, cc(), 1, {}, 0);
}
