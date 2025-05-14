/* { dg-do run { target { rv64 } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-require-effective-target riscv_zvfh_ok } */
/* { dg-options " -march=rv64gcv_zvfh -mabi=lp64d -O2" } */

#include "riscv_vector.h"

int8_t a[1];
uint16_t b[1];
float c[1], n[1];
uint16_t d[1];
uint8_t e[1];
uint16_t f[1];
_Float16 g[1], k[1], m[1], p[1];
uint16_t i[1];
int8_t j[1];
uint8_t o[1];
uint32_t l[1];
uint16_t q[1];
uint32_t r[1];
uint32_t s[1];
int16_t t[1];
int main()
{
  int u = 25;
  int8_t *v = a;
  uint32_t *w;
  uint16_t *aa = b;
  float *ab = c, *as = n;
  uint32_t *ad;
  uint16_t *ah = f;
  _Float16 *ai = g, *aj = k, *an = m, *au = p;
  int32_t *ak;
  int16_t *al;
  uint16_t *am = i;
  int8_t *ao = j;
  uint8_t *ap = o;
  uint32_t *aq = l;
  uint16_t *ar = q;
  uint32_t *at = r;
  uint32_t *av = s;
  int32_t *ax;
  int16_t *ay = t;
  for (size_t az; u; u -= az)
  {
    az = __riscv_vsetvl_e32m8(u);
    vint8m2_t ba = __riscv_vle8_v_i8m2(v, az);
    vbool4_t bb = __riscv_vmseq_vx_i8m2_b4(ba, 1, az);
    vuint16m4_t bc = __riscv_vsll_vx_u16m4(__riscv_vid_v_u16m4(az), 2, az);
    vuint32m8_t bd = __riscv_vsll_vx_u32m8(__riscv_vid_v_u32m8(az), 1, az);
    vuint32m8_t be = __riscv_vluxei16_v_u32m8_m(bb, w, bc, az);
    vuint16m4_t bf;
    __riscv_vsuxei16_v_u32m8_m(bb, aq, bf, be, az);
    vuint8m2_t bg = __riscv_vsll_vx_u8m2(__riscv_vid_v_u8m2(az), 1, az);
    vuint16m4_t bh = __riscv_vloxei8_v_u16m4(aa, bg, az);
    vfloat16m4_t bi;
    vuint16m4_t bj = __riscv_vsll_vx_u16m4(__riscv_vid_v_u16m4(az), 1, az);
    vint16m4_t bk = __riscv_vloxei32_v_i16m4_m(bb, al, bd, az);
    __riscv_vsse16_v_u16m4(ar, 2, bh, az);
    vuint16m4_t bl = __riscv_vloxei16_v_u16m4(d, bj, az);
    vfloat16m4_t bm = __riscv_vle16_v_f16m4(ai, az);
    vuint16m4_t bn = __riscv_vlse16_v_u16m4(ah, 2, az);
    vint32m8_t bo = __riscv_vle32_v_i32m8_m(bb, ak, az);
    vfloat16m1_t bp = __riscv_vle16_v_f16m1(aj, az);
    vuint16m4_t bq = __riscv_vrgatherei16_vv_u16m4(bl, bn, az);
    __riscv_vse16_v_u16m4(am, bq, az);
    vfloat16m1_t br = __riscv_vfredusum_vs_f16m4_f16m1_m(bb, bm, bp, az);
    vuint8m2_t bs;
    vuint32m8_t bt;
    __riscv_vse16_v_f16m1(an, br, az);
    vfloat32m8_t bu = __riscv_vloxei8_v_f32m8(ab, bs, az);
    __riscv_vse16_v_i16m4(ay, bk, az);
    bi = __riscv_vfmv_s_f_f16m4(1, az);
    __riscv_vse16_v_f16m4(au, bi, az);
    vuint16m4_t bw = __riscv_vsll_vx_u16m4(__riscv_vid_v_u16m4(az), 0, az);
    vuint32m8_t by = __riscv_vle32_v_u32m8_m(bb, ad, az);
    bt = __riscv_vmv_s_x_u32m8(3090659, az);
    __riscv_vse32_v_u32m8(at, bt, az);
    vuint8m2_t bz = __riscv_vloxei16_v_u8m2(e, bw, az);
    __riscv_vse32_v_u32m8(av, by, az);
    vint8m2_t cd;
    __riscv_vse8_v_i8m2(ao, cd, az);
    __riscv_vsse32_v_i32m8_m(bb, ax, 4, bo, az);
    __riscv_vse32_v_f32m8(as, bu, az);
    vuint16m4_t cf;
    __riscv_vsoxei16_v_u32m8(aq, cf, be, az);
    vuint8m2_t cg = __riscv_vmulhu_vx_u8m2(bz, 0, az);
    vuint32m8_t ch = __riscv_vsll_vx_u32m8(__riscv_vid_v_u32m8(az), 0, az);
    __riscv_vsoxei32_v_u8m2(ap, ch, cg, az);
  }
  return 0;
}
