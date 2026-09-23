/* { dg-do compile } */

#include <stdint.h>
typedef int16_t v4i16 __attribute__((vector_size(8)));
v4i16 g7, g19;
int64_t g16;
_Bool f16_a0_c3, f16_a0_ob6;
void f16_a0()
{
    int32_t ov10;
    uint64_t __ov_tmp_g16 = __builtin_sub_overflow(0, 0, &__ov_tmp_g16);
    g16 = __ov_tmp_g16;
lbl_b1:
    __builtin_mul_overflow(g16, g16, &ov10);
    switch (ov10)
    case 33:
        goto lbl_sw_def57;
lbl_sw_def15:
    if (f16_a0_c3) goto lbl_b1;
    g7 = __builtin_shufflevector(g7, g7, 4, 3, 4, 5);
    f16_a0_ob6 = f16_a0;
    g19 = g19 <= g19;
lbl_bf43:
    g19 = ~g19;
    g7 = __builtin_shufflevector(g7, g19, 6, 7, 0, 1);
lbl_sw_def57:
    if (f16_a0_ob6) goto lbl_sw_def15;
    goto lbl_bf43;
}
