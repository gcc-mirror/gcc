/* { dg-do compile } */
/* { dg-options "-std=c++17 -O2 -mfloat-abi=hard -mcpu=generic-armv7-a" } */
// { dg-require-effective-target arm_hard_ok }
// { dg-require-effective-target arm_neon_ok }
// { dg-add-options arm_neon }

#include <arm_neon.h>

typedef uint16x4_t e;
typedef int16x4_t f;
typedef int32x4_t g;
typedef float32x4_t h;
typedef uint32x4_t i;
g j, p;
g k(int l) { return vdupq_n_s32(l); }
i n(f l) { return (i)vmovl_u16((e)l); }
template <int, typename> struct q;
template <int r, typename aa> q<r, aa> operator<(aa s, q<r, aa> t) {
  return q<r, aa>(s) < t;
}
template <typename ab, typename ac, int r> q<r, ab> ad(const q<r, ac> &);
typedef q<4, int> ae;
template <> class q<4, float> {
 public:
 q(h af) : ag(af) {}
  q(float) {}
  static q ah(void *ai) {
    float *l = (float *)ai;
    return vld1q_f32(l);
  }
  q operator+(q o) {
    h l = ag, m = o.ag;
    return vaddq_f32(l, m);
  }
  q operator*(q) {
    h l = ag, m;
    return vmulq_f32(l, m);
  }
  h ag;
};
template <> class q<4, unsigned short> {
 public:
 q(f af) : ag(af) {}
  static q ah(void *ai) {
    unsigned short *l = (unsigned short *)ai;
    return (f)vld1_s16((int16_t *)l);
  }
  void aj() {
    f m = ag;
    vst1_u16(0, (e)m);
  }
  f ag;
};
template <> class q<4, int> {
 public:
 q(g af) : ag(af) {}
  q(int u) { ag = k(u); }
  static q ah(void *ai) {
    int32_t *l = (int32_t *)ai;
    return vld1q_s32(l);
  }
  q operator&(q o) {
    g v = ag & o.ag;
    return v;
  }
  q operator|(q o) {
    g w = ag | o.ag;
    return w;
  }
  q operator^(q) {
    g x = ag ^ p;
    return x;
  }
  q operator>>(int ak) { return ag >> q(ak).ag; }
  q operator<(q) {
    g y, z = j < ag;
    y = (g)z;
    return y;
  }
  g ag;
};
template <> ae ad(const q<4, unsigned short> &al) { return g(n(al.ag)); }
template <> q<4, unsigned short> ad(const ae &al) {
  i l(i(al.ag));
  return (f)vmovn_s32((g)l);
}
q<4, float> am(long long an) {
  q ao = q<4, unsigned short>::ah(&an);
  ae ak = ad<int>(ao), ap = ak & 8000, aq = ak ^ ap, ar = 55 < aq, as(aq);
  q at = as & ar;
  ae au = ap | at;
  return q<4, float>::ah(&au);
}
q<4, unsigned short> av(q<4, float> aw) {
  ae ak = ae::ah(&aw), ap = ak & 80000000, aq = ap, ax = 5, as = aq >> 3,
    ay = 6;
  q az = ax & as;
  ae au = ay | az;
  return ad<unsigned short>(au);
}
struct ba {
  typedef int bb;
  static q<4, float> bc(int s) { return am(s); }
};
q<4, float> bd(q<4, float> s) { return s * 0; }
template <typename be> void bf(void *bg, void *al, int bh, int bi) {
  int bj;
  auto bk(static_cast<typename be::bb *>(al) + bh),
    d = static_cast<typename be::bb *>(bg),
    bl = be::bc(static_cast<typename be::bb *>(al)[0]), bm = be::bc(0),
    c = bm;
  for (; bi;) {
    auto a = c, bn = be::bc(static_cast<typename be::bb *>(al)[1]),
      bo = be::bc(1);
    q bp = bn;
    q bq = bp;
    auto b = bq + bo;
    bl = be::bc(static_cast<typename be::bb *>(al)[2]);
    bm = be::bc(bk[2]);
    c = bl + bm;
    q br = a + b;
    auto bs = br;
    q bt = bd(bs);
    av(bt).aj();
    d[0] = bj;
  }
}
int bu;
void bv() { bf<ba>(0, 0, 0, bu); }
