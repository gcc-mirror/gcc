// PR target/85503
// { dg-do compile { target { { powerpc64le-*-* } && c++11 } } }
// { dg-require-effective-target powerpc_vsx_ok }
// { dg-options "-O2 -mvsx" }

namespace b {
template < int c > struct d { static constexpr int e = c; };
typedef d< false > f;
template < typename g > struct h { typedef g i; };
template < typename > class j;
template < bool, bool, typename... > struct aa;
struct k {
  template < typename n, typename... q >
  static h< decltype(n()(q()...)) > o(int);
};
template < typename p, typename... t > struct aa< false, false, p, t... > : k {
  typedef decltype(o< p, t... >(0)) i;
};
template < typename p, typename... t >
struct ab : aa< f::e, f::e, p, t... >::i {};
template < typename p, typename... t > struct j< p(t...) > : ab< p, t... > {};
} enum { r, s };
typedef long x;
template < typename > struct ac;
template < typename y > struct ac< y & > { typedef y i; };
template < typename y > struct j { typedef typename b::j< y >::i i; };
template < typename > struct ad;
template < typename > struct ae;
template < typename > class af;
template < typename, int ag, int ah, int = 0, int = ag, int = ah > class ai;
template < typename > class aj;
template < typename, typename, typename > class ak;
template < typename > class al;
template < typename > struct am;
template < typename > struct an;
template < typename y > struct ao { typedef typename am< y >::i i; };
struct aq {
  typedef const ai< double, 2, 1 > &ar;
};
template < typename as > struct at { typedef aj< as > i; };
template < typename as > struct au { typedef typename at< as >::i i; };
template < typename av > av aw(const typename an< av >::i *);
template < typename av, int > av az(const typename an< av >::i *ba) {
  return aw< av >(ba);
}
typedef __attribute__((altivec(vector__))) double bb;
template <> struct am< double > { typedef bb i; };
template <> struct an< bb > { typedef double i; };
template <> bb aw(const double *ba) { return __builtin_vec_vsx_ld(0, ba); }
struct bc {
  template < typename av > int bd(av a) { bd(a); __builtin_unreachable (); }
};
struct be {
  double operator()(const int, const int);
};
template < typename as > class bf : public ae< as > {
public:
  typedef typename ad< as >::bg bg;
  using ae< as >::bh;
  enum { bi };
  bg bj() const;
  template < typename bk > bg bl(const bk &) const;
};
template < typename as > class aj : public bf< as > {
public:
  using bf< as >::bh;
  template < typename bm > ak< be, const as, const bm > operator-(bm bn) {
    return ak< be, const as, const bm >(bh(), bn);
  }
  int bo() const;
  al< as > array() { return bh(); }
};
template < typename as > struct ae {
  const as &bh() const { return *static_cast< const as * >(this); }
};
template < typename > struct bp;
template < typename > struct bq;
template < typename y > struct br : bq< y > {
  br(y bs) : bq< y >(bs) {}
};
template < typename y > struct br< const y > : br< y > {
  br(const y &bs) : br< y >(bs) {}
};
template < typename as > struct br< af< as > > {
  typedef as bt;
  br(const bt &m) : bu(m.bv()) {}
  template < int bw, typename ay > ay bx(x, x by) {
    return az< ay, bw >(bu + by);
  }
  const typename bt::bg *bu;
};
template < typename bg, int u, int bz, int ca, int cb, int cc >
struct br< ai< bg, u, bz, ca, cb, cc > > : br< af< ai< bg, u, bz > > > {
  typedef ai< bg, u, bz > cd;
  br(const cd &m) : br< af< cd > >(m) {}
};
template < typename bk, typename ce, typename cf >
struct br< ak< bk, ce, cf > > : bp< ak< bk, ce, cf > > {
  br(ak< bk, ce, cf > bs) : bp< ak< bk, ce, cf > >(bs) {}
};
template < typename bk, typename ce, typename cf >
struct bp< ak< bk, ce, cf > > {
  bp(ak< bk, ce, cf > bs) : cg(), ch(bs.ci()) {}
  template < int bw, typename ay > ay bx(x cj, x by) {
    return ch.template bx< bw, ay >(cj, by);
  }
  ce cg;
  br< cf > ch;
};
template < typename cd > struct v {
  typedef typename ac< typename cd::ck >::i cl;
  v(cl &arg) : cn(arg) {}
  template < int bw, typename ay > ay bx(x cj, x by) {
    return cn.template bx< bw, ay >(cj, by);
  }
  br< cl > cn;
};
template < typename cm > struct bq< al< cm > > : v< al< cm > > {
  bq(al< cm > w) : v< al< cm > >(w.dd()) {}
};
template < typename as > class ap : public bf< as > {};
template < int ax > struct co { double array[ax]; };
template < int ax > class cq {
  co< ax > bu;

public:
  const double *bv() const { return bu.array; }
};
template < typename as > class af : public at< as >::i {
public:
  typedef typename at< as >::i cp;
  cq< cp::bi > cs;
  const typename ad< as >::bg *bv() const { return cs.bv(); }
};
template < typename cr, int ag, int ah, int cu, int ct, int cw >
struct ad< ai< cr, ag, ah, cu, ct, cw > > {
  typedef cr bg;
};
template < typename, int ag, int ah, int, int, int >
class ai : public af< ai< double, ag, ah > > {
public:
  typedef ai cv;
};
template < typename bk, typename ce, typename cf >
struct ad< ak< bk, ce, cf > > {
  typedef typename j< bk(typename ce::bg, typename cf::bg) >::i bg;
};
template < typename, typename, typename > class z;
template < typename bk, typename cy, typename cx >
class ak : public z< bk, cy, cx > {
public:
  typedef cx RhsNested;
  ak(cy, cx cz) : da(), db(cz) {}
  RhsNested ci() { return db; }
  cy da;
  RhsNested db;
};
template < typename bk, typename ce, typename cf >
class z : public au< ak< bk, ce, cf > >::i {};
template < typename as > int aj< as >::bo() const { (*this).bj(); __builtin_unreachable (); }
template < typename as > struct dc {
  typedef typename ao< typename as::bg >::i ay;
  enum { d, de };
  enum { df = de };
};
template < typename as > struct dk {
  enum { dh, di, dj, alignment };
  typedef typename dc< as >::ay PacketScalar;
  static PacketScalar dl(as dm, bc) {
    return dm.template dn< alignment, PacketScalar >(di, dj);
  }
};
template < typename, typename as, int = dc< as >::df > struct redux_impl;
template < typename Func, typename as > struct redux_impl< Func, as, s > {
  static int dl(as dm, Func func) { func.bd(dk< as >::dl(dm, func)); __builtin_unreachable (); }
};
template < typename _XprType > class redux_evaluator {
public:
  typedef _XprType cd;
  redux_evaluator(cd bs) : m_evaluator(bs), m_xpr(bs) {}
  typedef typename cd::bg bg;
  template < int bw, typename ay > ay dn(x di, x dj) {
    return m_evaluator.template bx< bw, ay >(dj, di);
  }
  br< cd > m_evaluator;
  cd m_xpr;
};
template < typename as >
template < typename Func >
typename ad< as >::bg bf< as >::bl(const Func &func) const {
  typedef redux_evaluator< as > ThisEvaluator;
  ThisEvaluator thisEval(bh());
  redux_impl< Func, ThisEvaluator >::dl(thisEval, func);
  __builtin_unreachable ();
}
template < typename as > typename ad< as >::bg bf< as >::bj() const {
  bl(bc());
  __builtin_unreachable ();
}
template < typename ExpressionType >
struct ad< al< ExpressionType > > : ad< typename ExpressionType::cv > {};
template < typename > class al : public ap< al< ai< double, 2, 1 > > > {
public:
  typedef aq::ar ck;
  al(const ai< double, 2, 1 > &dg) : m_expression(dg) {}
  const ai< double, 2, 1 > &dd() { return m_expression; }
  ck m_expression;
};
typedef ai< double, 2, 1 > Vector2d;
class OnHoverHandlerGraphicsItem {
public:
  static Vector2d corners;
};
int
GasGraphicsItemcreateOnHoverHandler() {
  Vector2d l;
  (l - OnHoverHandlerGraphicsItem::corners.array()).bo();
  __builtin_unreachable ();
}
