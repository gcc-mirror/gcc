/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
typedef int a;
using c = float;
template < typename > using e = int;
#pragma riscv intrinsic "vector"
template < typename, int, int f > struct aa {
  using g = int;
  template < typename > static constexpr int h() { return f; }
  template < typename i > using ab = aa< i, 0, h< i >() >;
};
template < int f > struct p { using j = aa< float, 6, f >; };
template < int f > struct k { using j = typename p< f >::j; };
template < typename, int f > using ac = typename k< f >::j;
template < class ad > using l = typename ad::g;
template < class g, class ad > using ab = typename ad::ab< g >;
template < class ad > using ae = ab< e< ad >, ad >;
template < int m > vuint32mf2_t ai(aa< a, m, -1 >, a aj) {
  return __riscv_vmv_v_x_u32mf2(aj, 0);
}
template < int m > vfloat32mf2_t ai(aa< c, m, -1 >, c);
template < class ad > using ak = decltype(ai(ad(), l< ad >()));
template < class ad > ak< ad > al(ad d) {
  ae< decltype(d) > am;
  return an(d, ai(am, 0));
}
template < typename g, int m > vuint8mf2_t ao(aa< g, m, -1 >, vuint32mf2_t n) {
  return __riscv_vreinterpret_v_u32mf2_u8mf2(n);
}
template < int m > vuint32mf2_t ap(aa< a, m, -1 >, vuint8mf2_t n) {
  return __riscv_vreinterpret_v_u8mf2_u32mf2(n);
}
template < typename g, int m > vuint8mf2_t ao(aa< g, m, -1 >, vfloat32mf2_t n) {
  return __riscv_vreinterpret_v_u32mf2_u8mf2(
      __riscv_vreinterpret_v_f32mf2_u32mf2(n));
}
template < int m > vfloat32mf2_t ap(aa< c, m, -1 >, vuint8mf2_t);
template < class ad, class aq > ak< ad > an(ad d, aq n) {
  return ap(d, ao(d, n));
}
vbool64_t av(vuint32mf2_t, vuint32mf2_t);
template < class ad > bool ba(ad, vbool64_t);
template < class ad > using bb = decltype(al(ad()));
template < typename g > using be = ac< g, -1 >;
struct bf {
  template < class ad > bool bh(ad, bb< ad > bi) {
    ae< ad > am;
    return ba(am, av(an(am, bi), al(am)));
  }
};
int bo;
template < class ad, class bl, typename g > void o(ad d, bl bn, g) {
  bb< ad > bq = al(d);
  for (; bo;) {
    int br = bn.bh(d, bq);
    if (__builtin_expect(br, 0))
      for (;;)
        ;
  }
}
template < class ad, class bl, typename g > void bs(ad d, bl bn, g) {
  g bu;
  o(d, bn, bu);
}
template < class ad, class bl, typename g >
void bv(ad d, bl bn, g *, int, g *bt) {
  bs(d, bn, bt);
}
float by;
int bz;
float ca;
void b() {
  be< float > d;
  bf bn;
  bv(d, bn, &by, bz, &ca);
}
