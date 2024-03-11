/* { dg-do compile { target { riscv_v } } } */

#include <iostream>
#include "riscv_vector.h"
using std::cerr;
using std::endl;
template < class , class b > int c(b val) {
  return val;
}
auto &f32(c< float, uint32_t >);
template < class d >
bool check(d , d , size_t );
int main() {
  size_t e ;
  int16_t f[] {};
  size_t g  ;
  int32_t i[] {4784};
  size_t aa = 4;
  int16_t ab[] {2313};
  int16_t j[] {7114 };
  int16_t k[] {7696 };
  uint32_t l[] {9951 };
  int32_t m[] {2659 };
  uint16_t n[] {7537 };
  int32_t o[] {05733}
    ;
  uint32_t p[] {7010090 };
  uint32_t q[] {21060 };
  uint32_t r[] {2273 };
  uint32_t s[] {4094366 };
  int16_t ac[] {11880 };
  int16_t t[] {10988};
  int16_t ad[] {30376};
  int8_t u[] {};
  int8_t ae[] {7};
  int8_t v[] {40};
  int8_t af[] {6};
  int16_t w[]  {4077 };
  int16_t x[]  {7932 };
  int8_t y[] {3};
  int8_t z[] {4};
  uint16_t ag[] {2831};
  int16_t ah[] {10412 };
  int16_t ai[] {6823};
  int32_t aj[] {8572 };
  int32_t ak[] {9999 };
  uint32_t al[] {50166962 };
  uint32_t am[] {9781 };
  int8_t an[] {9, 35};
  float ao[] {222.65, 22.79};
  float ap[] {126.10, 13.92};
  int64_t aq[] {508727, 5556};
  int16_t ar[] {2861 };
  int16_t as[] {21420};
  int16_t at[] {4706 };
  uint32_t au ;
  uint32_t av = 600295662;
  size_t aw ;
  int16_t ax = 13015;
  uint32_t ay ;
  uint16_t az = 10652;
  int32_t ba ;
  int8_t bb ;
  int64_t bc = 40183771683589512;

asm volatile ("ttt":::"memory");
  vint16mf4_t bd = __riscv_vle16_v_i16mf4(j, 2);
  vuint32mf2_t be = __riscv_vle32_v_u32mf2(l, 2);
  vint32mf2_t bf = __riscv_vle32_v_i32mf2(m, 2);
  vuint16mf4_t bg = __riscv_vle16_v_u16mf4(n, 2);
  vint8mf4_t bh ;
  vuint32m2_t bi = __riscv_vle32_v_u32m2(p, 2);
  vuint32m2_t bj = __riscv_vle32_v_u32m2(q, 2);
  vuint32m2_t bk = __riscv_vle32_v_u32m2(r, 2);
  vuint32m2_t bl = __riscv_vle32_v_u32m2(s, 2);
  vint16m1_t bm = __riscv_vle16_v_i16m1(ac, 2);
  vint16m1_t bn = __riscv_vle16_v_i16m1(t, 2);
  vint8mf2_t bo = __riscv_vle8_v_i8mf2(u, 1);
  vint8mf2_t bp = __riscv_vle8_v_i8mf2(ae, 1);
  vint8mf8_t bq = __riscv_vle8_v_i8mf8(af, 1);
  vint16mf4_t br = __riscv_vle16_v_i16mf4(w, 2);
  vint16mf4_t bs = __riscv_vle16_v_i16mf4(x, 2);
  vint8mf8_t bt = __riscv_vle8_v_i8mf8(y, 1);
  vint8mf8_t bu = __riscv_vle8_v_i8mf8(z, 1);
  vuint16mf4_t bv = __riscv_vle16_v_u16mf4(ag, 1);
  vint16mf4_t bw = __riscv_vle16_v_i16mf4(ah, 2);
  vint16mf4_t bx = __riscv_vle16_v_i16mf4(ai, 2);
  vint32mf2_t by = __riscv_vle32_v_i32mf2(aj, 2);
  vint32mf2_t bz = __riscv_vle32_v_i32mf2(ak, 2);
  vuint32mf2_t ca = __riscv_vle32_v_u32mf2(al, 2);
  vuint32mf2_t cb = __riscv_vle32_v_u32mf2(am, 2);
  vint8mf8_t cc = __riscv_vle8_v_i8mf8(an, 2);
  vfloat32mf2_t cd = __riscv_vle32_v_f32mf2(ao, 2);
  vfloat32mf2_t ce = __riscv_vle32_v_f32mf2(ap, 2);
  vint64m1_t cf = __riscv_vle64_v_i64m1(aq, 2);
  vint16mf4_t cg = __riscv_vle16_v_i16mf4(ar, 2);
  vint16mf4_t ch = __riscv_vle16_v_i16mf4(as, 2);
  vint16mf4_t var_62 = __riscv_vle16_v_i16mf4(at, 2);
  vbool64_t var_20 = __riscv_vmadc_vx_u32mf2_b64(be, ay, 2);
  int8_t var_17 = __riscv_vmv_x_s_i8mf4_i8(bh);
  vbool16_t var_28 = __riscv_vmsltu_vv_u32m2_b16(bk, bl, 2);
  vint8mf2_t var_14 = __riscv_vadd_vv_i8mf2(bo, bp, 1);
  vbool64_t var_8 = __riscv_vmseq_vv_i16mf4_b64(br, bs, 2);
  vbool64_t var_42 = __riscv_vmsbc_vx_u16mf4_b64(bv, az, 1);
  vbool64_t var_46 = __riscv_vmsge_vx_i32mf2_b64(by, ba, 2);
  vint16mf4_t var_4 = __riscv_vncvt_x_x_w_i16mf4(bz, 2);
  vbool64_t var_51 = __riscv_vmsgt_vx_i8mf8_b64(cc, bb, 2);
  vbool64_t var_56 = __riscv_vmfne_vv_f32mf2_b64(cd, ce, 2);
  vbool64_t var_55 = __riscv_vmseq_vx_i64m1_b64(cf, bc, 2);
  vuint32m2_t var_16 = __riscv_vslideup_vx_u32m2_mu(var_28, bi, bj, aw, 2);
  vint8mf2_t var_12 = __riscv_vmulh_vv_i8mf2(var_14, var_14, 1);
  vint16mf4_t var_0 = __riscv_vdiv_vv_i16mf4_mu(var_8, var_4, ch, var_62, 2);
  vuint32m2_t var_13 = __riscv_vsub_vx_u32m2(var_16, av, 2);
  int8_t var_9 = __riscv_vmv_x_s_i8mf2_i8(var_12);
  vint16mf4_t var_19 = __riscv_vor_vx_i16mf4_mu(var_20, var_0, bd, ax, 2);
  uint32_t var_10 = __riscv_vmv_x_s_u32m2_u32(var_13);
  vint8mf8_t var_7 = __riscv_vmadd_vx_i8mf8_mu(var_42, bt, var_9, bu, 1);
  __riscv_vse16_v_i16mf4(k, var_19, 2);
  vuint32mf2_t var_3 =
      __riscv_vslide1down_vx_u32mf2_mu(var_51, ca, cb, var_10, 2);
  if (check(k, ab, aa))
    cerr << "check 8 fails" << endl;
  vbool64_t var_2 = __riscv_vmsne_vx_u32mf2_b64_mu(var_55, var_56, var_3, au, 2);
  vint16mf4_t var_1 = __riscv_vssub_vv_i16mf4_mu(var_2, var_0, var_4, cg, 2);
  vint16mf4_t var_5 = __riscv_vxor_vv_i16mf4_mu(var_46, var_1, bw, bx, 2);
  vint32mf2_t var_18 = __riscv_vwmaccsu_vv_i32mf2(bf, var_1, bg, 2);
  vint8mf8_t var_6 = __riscv_vncvt_x_x_w_i8mf8_mu(var_8, var_7, var_5, 1);
  vint16m1_t var_15 = __riscv_vredand_vs_i16mf4_i16m1_tu(bm, var_5, bn, 2);
  __riscv_vse32_v_i32mf2(o, var_18, 2);
  vbool64_t var_11 = __riscv_vmsge_vx_i8mf8_b64(var_6, var_17, 1);
  __riscv_vse16_v_i16m1(ad, var_15, 1);
  if (check(o, i, g))
    cerr << "check 1 fails" << endl;
  __riscv_vse8_v_i8mf8_m(var_11, v, bq, 1);
  if (check(ad, f, e))
    cerr << "check 4 fails" << endl;
  cerr << "check 7 fails" << endl;
  return 0;
}
