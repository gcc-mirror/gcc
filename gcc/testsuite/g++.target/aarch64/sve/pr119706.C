/* { dg-do compile } */
/* { dg-options "-O3 -mcpu=neoverse-v2 --param=aarch64-autovec-preference=sve-only -w" } */

namespace a {
typedef long unsigned b;
typedef int c;
template <bool, typename d> struct e { using f = d; };
template <bool g, typename d = void> using h = typename e<g, d>::f;
template <typename aa, typename, template <typename> class> struct i {
  using f = aa;
};
template <typename aa, template <typename> class j> using k = i<aa, void, j>;
template <typename aa, template <typename> class j>
using l = typename k<aa, j>::f;
} // namespace a
inline void *operator new(a::b, void *ab) { return ab; }
namespace a {
template <typename> class ac {
public:
  typedef b m;
  template <typename ad, typename... n> void ae(ad *ab, n... w) {
    new (ab) ad(w...);
  }
};
template <typename d> using x = ac<d>;
template <typename d> class af : public x<d> {
public:
  typedef d o;
  template <typename> struct ag { typedef af ah; };
};
struct ai {};
struct aj : ai {};
struct ak : aj {};
template <typename> struct al;
template <typename d> struct al<d *> {
  typedef ak an;
  typedef c ao;
  typedef d ap;
};
template <typename aq> typename aq ::an ar(aq) { return typename aq ::an(); }
template <typename as> typename as ::ao at(as au, as av, ak) { return av - au; }
template <typename aw> typename aw ::ao ax(aw au, aw av) {
  return at(au, av, ar(au));
}
template <typename> struct ay { typedef c ao; };
} // namespace a
namespace az {
template <typename am, typename> class ba {
  am bb;
  typedef a::al<am> bc;

public:
  typedef typename bc::an an;
  typedef typename bc::ao ao;
  typedef typename bc::ap ap;
  ba(am bd) : bb(bd) {}
  ap operator*() { return *bb; }
  ba operator++() {
    ++bb;
    return *this;
  }
  am base() { return bb; }
};
template <typename be, typename bf, typename bg>
bool operator!=(ba<be, bg> bh, ba<bf, bg> p) {
  return bh.base() != p.base();
}
template <typename be, typename bf, typename bg>
auto operator-(ba<be, bg> bh, ba<bf, bg> p) {
  return bh.base() - p.base();
}
} // namespace az
namespace a {
struct bi {
  template <typename d, typename> struct bj {
    using f = typename d::ag<d>::ah;
  };
  template <typename> using bk = b;
  template <typename...> static constexpr bool bl = false;
  template <typename, typename> static constexpr bool bm = bl<>;
  template <typename d, typename... n> static constexpr bool bn = bm<d, n...>;
};
template <typename bo, typename ad> using bp = typename bi::bj<bo, ad>::f;
template <typename bo> struct bq : bi {
  typedef typename bo::o o;
  using br = l<o *, bk>;
  template <typename, typename bs> struct bt { using f = typename ay<bs>::ao; };
  template <typename bu, typename> struct bv { using f = typename bu::m; };
  using ao = typename bt<bo, c>::f;
  using m = typename bv<bo, ao>::f;
  template <typename d> using bw = bp<bo, d>;
  static br allocate(bo, m);
  template <typename d, typename... n>
  static h<bn<bo, d>> ae(bo ci, d ab, n... w) {
    ci.ae(ab, w...);
  }
};
template <typename d> struct bx {
  static bool by(d &bz) try { d(bz.begin(), bz.ca(), bz.cb()); } catch (...) {
  }
};
} // namespace a
namespace az {
template <typename bo> struct cc : a::bq<bo> {
  typedef a::bq<bo> q;
  template <typename d> struct ag { typedef typename q::bw<d> ah; };
};
} // namespace az
enum cd {};
using ce = double;
namespace a {
template <typename aw, typename cf, typename cg, typename ch>
cg cj(aw au, cf av, cg ck, ch cl) {
  typedef az::cc<ch> cx;
  for (; au != av; ++au, ++ck)
    cx::ae(cl, ck, *au);
}
template <typename d, typename bo> struct cm {
  typedef typename az::cc<bo>::ag<d>::ah cn;
  typedef typename az::cc<cn>::br br;
  struct co {
    br db;
    br cp;
  };
  struct cq : cn, co {
    cq(cn) {}
  } typedef cr;
  cn cs();
  cr cb() noexcept;
  cm(cr ci) : ct(ci) {}
  cq ct;
  br cu(b cv) {
    typedef az::cc<cn> cw;
    return cv ? cw::allocate(ct, cv) : c();
  }
};
template <typename d, typename bo = af<d>> class cy : cm<d, bo> {
  typedef cm<d, bo> cz;

public:
  typedef typename cz::br br;
  typedef az::ba<br, cy> da;
  typedef b m;
  typedef bo cr;
  cz::cs;
  template <typename aw> cy(aw au, aw av, cr ci) : cz(ci) {
    dg(au, av, ar(au));
  }
  cz::cb;
  da begin() { return this->ct.db; }
  da ca() { return this->ct.cp; }
  void r() { s(); }
  void clear() { t(this->ct.db); }
  template <typename cg> void dg(cg au, cg av, ai) { y(au, av, ax(au, av)); }
  template <typename am, typename cf> void y(am au, cf av, m cv) {
    br z = this->cu(dc(cv, cs()));
    cj(au, av, z, cs());
  }
  bool s();
  m dc(m cv, cr) { return cv; }
  void t(br dd) {
    if (this->ct.cp - dd)
      this->ct.cp = dd;
  }
};
template <typename d, typename bo> bool cy<d, bo>::s() { bx<cy>::by(*this); }
namespace basic {
class u {
  using de = ce;
  void v(cd, b);
  cy<de> df;
};
void u::v(cd, b) {
  df.clear();
  df.r();
}
} // namespace basic
} // namespace a