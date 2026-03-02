/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O0" } */

#include <riscv_vector.h>
#include <vector>
#define a 36

uint8_t e[a], x[a];
int64_t f[a], g[a], l[a];
float j[a], k[a], m[a];

int main() {
  for (int i = 0; i < a; ++i) { e[i]=1; g[i] = 86; x[i] = 86; }
  for (size_t n = 0, avl = a; avl;) {
    size_t o = __riscv_vsetvl_e64m8(avl);
    vuint8m1_t p = __riscv_vle8_v_u8m1(&e[n], o);
    vbool8_t q = __riscv_vmseq_vx_u8m1_b8(p, 1, o);
    vuint64m8_t r = __riscv_vsll_vx_u64m8(__riscv_vid_v_u64m8(o), 3, o);
    vint64m8_t s = __riscv_vluxei64_v_i64m8_tum(
        __riscv_vlm_v_b8(std::vector<uint8_t>(o + 7).data(), o),
        __riscv_vmv_v_x_i64m8(0, __riscv_vsetvlmax_e16m2()), &f[n], r, o);
    vuint32m4_t t = __riscv_vsll_vx_u32m4(__riscv_vid_v_u32m4(o), 3, o);
    vint64m8_t u = __riscv_vluxei32(&g[n], t, o);
    vbool8_t v = __riscv_vlm_v_b8(&x[n], o);
    __riscv_vle32ff_v_f32m4_mu(q, __riscv_vfmv_v_f_f32m4(0, __riscv_vsetvlmax_e8m1()), &j[n], &o, o);
    vfloat32m1_t w = __riscv_vfmv_v_f_f32m1(0, __riscv_vsetvlmax_e32m1());
    vfloat32m1_t aa = __riscv_vle32_v_f32m1_tu(w, &k[n], o);
    s = __riscv_vcompress_vm_i64m8_tu(s, u, v, o);
    vfloat32mf2_t ab = __riscv_vlmul_trunc_v_f32m1_f32mf2(aa);
    vuint64m8_t ac = __riscv_vsll_vx_u64m8(__riscv_vid_v_u64m8(o), 3, o);
    __riscv_vsuxei64_v_i64m8(&l[n], ac, s, o);
    __riscv_vse32_v_f32mf2(&m[n], ab, o);
    avl -= o;
  }

  /* Results are inconsistent between different VLENs.
     "n" never changes so we will always store into l[0...] with a length of
     "o".  What differs is "s".
     At zvl128b and zvl256b we have more than one loop iteration and
     "s" will be {86, 86, -1, -1} or {86, 86, 0, 0} depending on the
     tail/mask policy.
     At zvl512b there is only one iteration and s = {86, 86, 86, ...}.
     I cross checked with clang and this seems correct.
     Therefore only check l's fifth element.
     The actual PR is about fault-only-first loads and the wrong code
     caused element 5 to be incorrect as well.  */
  if (l[5] != 86)
    __builtin_abort ();
}
