// { dg-do compile }
// { dg-options "-O3 -std=gnu++1y -w" }

namespace {
template <typename b> b c(b);
template <typename d, typename, template <typename> class> struct f {
  using g = d;
};
template <typename d, template <typename> class aa> using h = f<d, void, aa>;
template <typename d, template <typename> class aa>
using i = typename h<d, aa>::g;
template <typename b> struct j { typedef b k; };
} // namespace
namespace l {
template <typename b> class m {
public:
  typedef b k;
};
} // namespace l
namespace a {
template <typename b> using n = l::m<b>;
template <typename b> class ac : public n<b> {};
struct s {
  template <typename b> using ad = typename b::e;
};
template <typename o> struct p : s {
  typedef typename o::k k;
  using ag = i<k *, ad>;
};
} // namespace a
namespace l {
template <typename o> struct t : a::p<o> {};
} // namespace l
namespace a {
template <bool> struct al {
  template <typename am> static void an(am ao, am) { c(*ao); }
};
template <typename am> void aq(am ao, am ap) {
  typedef typename j<am>::k ar;
  al<__has_trivial_destructor(ar)>::an(ao, ap);
}
namespace {
typedef char au;
}
} // namespace a
typedef char av;
typedef int aw;
typedef av ay;
typedef aw az;
namespace a {
template <typename, typename o> struct ba {
  typedef typename l::t<o>::ag ag;
  struct {
    ag bb;
    ag bc;
  } bd;
};
template <typename b, typename o = ac<b>> class be : ba<b, o> {
  typedef ba<b, o> bf;
  typedef typename bf::ag ag;

public:
  void bh() { bi(this->bd.bb); }
  void bi(ag bj) { aq(bj, this->bd.bc); }
};
} // namespace a
namespace bk {
enum bl {};
enum bn { bo };
class q {
public:
  static a::au bp(bn);
  static bool bq(a::au *br, bn g) { return bs(br, g); }
  static bl bs(a::au *br, bn g) {
    if (br) {
      auto bt = bp(g);
      if (bt)
        return bl();
    }
  }
};
template <typename, typename> class bu {};
} // namespace bk
namespace bv {
namespace bw {
class bx;
}
} // namespace bv
namespace bk {
enum by { bz };
struct ca;
class cb {
public:
  class cc {
  public:
    virtual void cd(by) = 0;
  };
  virtual bu<ca, by> e();
  cc *cf;
};
class cg {
public:
  ~cg() { q::bq(ch, bo); }
  a::au *ch;
};
class ci {
  cg cj;
};
namespace ck {
enum cl : ay;
}
class r : ci {};
class cn {
public:
  ck::cl co();
};
by cp(ck::cl);
class cq : cb, cb::cc {
  bu<ca, by> ce(bv::bw::bx &, az) noexcept;
  void cd(by);
  void cr(bv::bw::bx &, az, cb::cc *) noexcept;
  cn cs;
  a::be<r> ct;
};
} // namespace bk
using bv::bw::bx;
namespace bk {
bu<ca, by> cq::ce(bx &, az) noexcept { ct.bh(); }
void cq::cr(bx &, az, cb::cc *) noexcept { cd(bz); }
void cq::cd(by) { cf->cd(cp(cs.co())); }
} // namespace bk
