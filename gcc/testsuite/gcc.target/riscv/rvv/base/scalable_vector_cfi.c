/* { dg-do compile } */
/* { dg-options "-g -O3 -march=rv64gcv -mabi=lp64d" } */
/* { dg-skip-if "" { *-*-* } {"-O2" "-O1" "-O0" "-Og" "-Oz" "-flto"} } */
/* { dg-final { scan-assembler {cfi_escape .*0x92,0xa2,0x38,0,0x32,0x1e} } } */

#include "riscv_vector.h"

#define PI_2 1.570796326795

extern void func(float *result);

void test(const float *ys, const float *xs, float *result, size_t length) {
    size_t gvl = __riscv_vsetvlmax_e32m2();
    vfloat32m2_t vpi2 = __riscv_vfmv_v_f_f32m2(PI_2, gvl);

    for(size_t i = 0; i < length;) {
        gvl = __riscv_vsetvl_e32m2(length - i);
        vfloat32m2_t y = __riscv_vle32_v_f32m2(ys, gvl);
        vfloat32m2_t x = __riscv_vle32_v_f32m2(xs, gvl);
        vbool16_t mask0  = __riscv_vmflt_vv_f32m2_b16(x, y, gvl);
        vfloat32m2_t fixpi = __riscv_vfrsub_vf_f32m2_mu(mask0, vpi2, vpi2, 0, gvl);

        __riscv_vse32_v_f32m2(result, fixpi, gvl);

        func(result);

        i += gvl;
        ys += gvl;
        xs += gvl;
        result += gvl;
    }
}
