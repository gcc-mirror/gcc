/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

void
test_vbool1_then_vbool1(int8_t * restrict in, int8_t * restrict out) {
    vbool1_t v1 = *(vbool1_t*)in;
    vbool1_t v2 = *(vbool1_t*)in;

    *(vbool1_t*)(out + 100) = v1;
    *(vbool1_t*)(out + 200) = v2;
}

void
test_vbool2_then_vbool2(int8_t * restrict in, int8_t * restrict out) {
    vbool2_t v1 = *(vbool2_t*)in;
    vbool2_t v2 = *(vbool2_t*)in;

    *(vbool2_t*)(out + 100) = v1;
    *(vbool2_t*)(out + 200) = v2;
}

void
test_vbool4_then_vbool4(int8_t * restrict in, int8_t * restrict out) {
    vbool4_t v1 = *(vbool4_t*)in;
    vbool4_t v2 = *(vbool4_t*)in;

    *(vbool4_t*)(out + 100) = v1;
    *(vbool4_t*)(out + 200) = v2;
}

void
test_vbool8_then_vbool8(int8_t * restrict in, int8_t * restrict out) {
    vbool8_t v1 = *(vbool8_t*)in;
    vbool8_t v2 = *(vbool8_t*)in;

    *(vbool8_t*)(out + 100) = v1;
    *(vbool8_t*)(out + 200) = v2;
}

void
test_vbool16_then_vbool16(int8_t * restrict in, int8_t * restrict out) {
    vbool16_t v1 = *(vbool16_t*)in;
    vbool16_t v2 = *(vbool16_t*)in;

    *(vbool16_t*)(out + 100) = v1;
    *(vbool16_t*)(out + 200) = v2;
}

void
test_vbool32_then_vbool32(int8_t * restrict in, int8_t * restrict out) {
    vbool32_t v1 = *(vbool32_t*)in;
    vbool32_t v2 = *(vbool32_t*)in;

    *(vbool32_t*)(out + 100) = v1;
    *(vbool32_t*)(out + 200) = v2;
}

void
test_vbool64_then_vbool64(int8_t * restrict in, int8_t * restrict out) {
    vbool64_t v1 = *(vbool64_t*)in;
    vbool64_t v2 = *(vbool64_t*)in;

    *(vbool64_t*)(out + 100) = v1;
    *(vbool64_t*)(out + 200) = v2;
}

/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*m8,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*m4,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*m2,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*m1,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*mf2,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*mf4,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x][0-9]+,\s*zero,\s*e8,\s*mf8,\s*ta,\s*ma} 1 } } */
/* { dg-final { scan-assembler-times {vlm\.v\s+v[0-9]+,\s*0\([a-x][0-9]+\)} 7 } } */
/* { dg-final { scan-assembler-times {vsm\.v\s+v[0-9]+,\s*0\([a-x][0-9]+\)} 14 } } */
