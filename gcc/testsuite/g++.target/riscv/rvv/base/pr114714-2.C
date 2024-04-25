/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -std=c++17" } */

typedef int a;
typedef short b;
typedef unsigned c;
template < typename > using e = unsigned;
template < typename > void ab();
#pragma riscv intrinsic "vector"
template < typename f, int, int ac > struct g {
  using i = f;
  template < typename m > using j = g< m, 0, ac >;
  using k = g< i, 1, ac - 1 >;
  using ad = g< i, 1, ac + 1 >;
};
namespace ae {
struct af {
  using h = g< short, 6, 0 < 3 >;
};
struct ag {
  using h = af::h;
};
} template < typename, int > using ah = ae::ag::h;
template < class ai > using aj = typename ai::i;
template < class i, class ai > using j = typename ai::j< i >;
template < class ai > using ak = j< e< ai >, ai >;
template < class ai > using k = typename ai::k;
template < class ai > using ad = typename ai::ad;
template < a ap > vuint16mf2_t ar(g< b, ap, 0 >, b);
template < a ap > vuint16m1_t ar(g< b, ap, 1 >, b);
template < a ap > vuint32m1_t ar(g< c, ap, 1 >, c);
template < a ap > vuint32m2_t ar(g< c, ap, 2 >, c);
template < class ai > using as = decltype(ar(ai(), aj< ai >()));
template < class ai > as< ai > at(ai);
namespace ae {
template < int ap > vuint32m2_t au(g< c, ap, 1 + 1 >, vuint32m1_t l) {
  return __riscv_vlmul_ext_v_u32m1_u32m2(l);
}
} template < int ap > vuint32m1_t aw(g< c, ap, 1 >, vuint16mf2_t l) {
  return __riscv_vzext_vf2_u32m1(l, 0);
}
namespace ae {
vuint32m2_t ax(vuint32m2_t, vuint32m2_t, a);
}
template < class ay, class an > as< ay > az(ay ba, an bc) {
  an bb;
  return ae::ax(ae::au(ba, bc), ae::au(ba, bb), 2);
}
template < class bd > as< bd > be(bd, as< ad< bd > >);
namespace ae {
template < class bh, class bi > void bj(bh bk, bi bl) {
  ad< decltype(bk) > bn;
  az(bn, bl);
}
} template < int ap, int ac, class bp, class bq >
void br(g< c, ap, ac > bk, bp, bq bl) {
  ae::bj(bk, bl);
}
template < class ai > using bs = decltype(at(ai()));
struct bt;
template < int ac = 1 > class bu {
public:
  template < typename i > void operator()(i) {
    ah< i, ac > d;
    bt()(i(), d);
  }
};
struct bt {
  template < typename bv, class bf > void operator()(bv, bf bw) {
    using bx = bv;
    ak< bf > by;
    k< bf > bz;
    using bq = bs< decltype(by) >;
    using bp = bs< decltype(bw) >;
    bp cb;
    ab< bx >();
    for (;;) {
      bp cc;
      bq bl = aw(by, be(bz, cc));
      br(by, cb, bl);
    }
  }
};
void d() { bu()(b()); }
