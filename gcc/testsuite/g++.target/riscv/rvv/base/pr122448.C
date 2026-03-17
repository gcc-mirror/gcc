/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-additional-options "-O3 -march=rv64gcv -mabi=lp64d -fdump-rtl-vsetvl-details" } */

#include <riscv_vector.h>
int a;
long b = -2260814313524985651LL;
short c; char d;
short e[576];
unsigned long long f;
void g(unsigned long long *i, unsigned long long ad) { *i = ad; }
int8_t j[4];
int16_t k[4], l[4];
void m() {
  for (short n = 1; n < 023; n += 4)
    for (short o = 0; o < static_cast<short>(1033314678U); o += 4)
      for (int p = (int)((long long)(b - 859406540) & 0xFFFFFFFF); p < 9; p += 3) {
        c ^= static_cast<short>(1033314678 % 0x10000);
        d &= static_cast<char>(a ? 0 : e[p * 24]);
      }
  for (bool q = 0; q < (bool)8; q = 1) {
    size_t r = 4;
    for (size_t v; r; r -= v) {
      v = __riscv_vsetvl_e16m1(r);
      vint8mf2_t w = __riscv_vle8_v_i8mf2(&j[0], v);
      vbool16_t ac = __riscv_vmseq_vx_i8mf2_b16(w, 1, v);
      vint16m1_t x = __riscv_vmv_v_x_i16m1(0, __riscv_vsetvlmax_e16m1());
      vuint16m1_t y = __riscv_vsll_vx_u16m1(__riscv_vid_v_u16m1(v), 1, v);
      vint16m1_t z = __riscv_vluxei16_v_i16m1_tu(x, &k[0], y, v);
      vint16m1_t aa = __riscv_vmax_vv_i16m1(z, z, v);
      vuint8mf2_t ab = __riscv_vsll_vx_u8mf2(__riscv_vid_v_u8mf2(v), 1, v);
      __riscv_vsoxei8_v_i16m1_m(ac, &l[0], ab, aa, v);
    }
  }
}

int main() {
  m();
  g(&f, d);
  if (f != 0)
    __builtin_abort ();
}

/* { dg-final { scan-rtl-dump-not "Hoisting vsetvl" "vsetvl" } } */
