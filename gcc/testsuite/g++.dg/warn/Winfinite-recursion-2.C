/* PR middle-end/88232 - Please implement -Winfinite-recursion
   Test case from PR 87742 (see PR 88232, comment 2.
   { dg-do compile { target c++11 } }
   { dg-options "-Wall -Winfinite-recursion" } */

namespace std
{
class type_info {
public:
  void k() const;
};

} // namespace std

using std::type_info;

template <int a> struct f { static constexpr int c = a; };
struct h {
  typedef int e;
};

template <unsigned long, typename...> struct m;
template <unsigned long ab, typename i, typename j, typename... ac>
struct m<ab, i, j, ac...> : m<ab + 1, i, ac...> {};
template <unsigned long ab, typename j, typename... ac>
struct m<ab, j, j, ac...> : f<ab> {};
template <unsigned long, typename...> struct n;
template <unsigned long ab, typename j, typename... ac>
struct n<ab, j, ac...> : n<ab - 1, ac...> {};
template <typename j, typename... ac> struct n<0, j, ac...> : h {};
template <typename... l> class F {
  template <typename i> struct I : m<0, i, l...> {};
  template <int ab> struct s : n<ab, l...> {};
  static const type_info *const b[];
  struct G {
    template <typename ag>
    operator ag() const       // { dg-warning "-Winfinite-recursion" }
    {
      return *this;
    }
  };
  unsigned o;
  G ah;

public:
  F();
  long t() const { return o; }
  const type_info &m_fn3() const { return *b[o]; }
  template <int ab> typename s<ab>::e *m_fn4() const {
    if (o != ab)
      return nullptr;
    return ah;
  }
  template <int ab> void m_fn5() const {
    m_fn4<ab>();
    const type_info &r = m_fn3();
    r.k();
  }
  template <typename i> void u() const { m_fn5<I<i>::c>(); }
};
template <typename... l> const type_info *const F<l...>::b[] {&typeid(l)...};
using am = unsigned char;
class H {
  enum bd : am { be = 2 };
  using bf = F<int, int, H>;
  bf ah;
  template <typename bg> void v() const { ah.u<bg>(); }
  void w() const;
};
void H::w() const {
  bd d = bd(ah.t());
  switch (d)
  case be:
    v<H>();
}
