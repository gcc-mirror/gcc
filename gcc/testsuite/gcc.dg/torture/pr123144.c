/* { dg-do compile } */
/* { dg-additional-options "-w" } */
/* { dg-additional-options "-mcpu=power8" { target powerpc64*-*-* } } */

#include <stdint.h>
#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
int8_t backsmith_snippet_893(int8_t BS_ARG_1, BS_VEC(uint8_t, 32) BS_ARG_5)
{
    BS_VEC(uint16_t, 8) BS_VAR_0;
    for (;;)
        for (uint64_t BS_INC_1 = 0; BS_INC_1 < 13; BS_INC_1 += 1)
        {
            if (BS_ARG_5[2])
                for (;;)
                    __builtin_convertvector((BS_VEC(int8_t, 16)){},
                                            BS_VEC(uint8_t, 16));
            uint8_t BS_TEMP_59 = __builtin_convertvector(
                __builtin_shufflevector(
                    __builtin_convertvector((BS_VEC(int8_t, 4)){},
                                            BS_VEC(uint16_t, 4)),
                    __builtin_convertvector((BS_VEC(int8_t, 4)){ BS_ARG_1 },
                                            BS_VEC(uint16_t, 4)),
                    4, 2, 2, 2, 6, 3, 0, 0, 3, 2, 1, 2, 4, 0, 1, 4, 2, 0, 3, 6,
                    4, 3, 1, 0, 2, 5, 3, 7, 4, 2, 4, 2),
                BS_VEC(uint8_t, 32))[(BS_INC_1 ? BS_VAR_0[3] : 2) ?: 2];
            uint8_t BS_TEMP_60 = BS_TEMP_59;
            for (; BS_TEMP_60;)
                ;
            BS_VEC(uint32_t, 8)
            BS_TEMP_68 = __builtin_convertvector(
                __builtin_convertvector(
                    (BS_VEC(uint64_t, 8)){ BS_INC_1, BS_INC_1, BS_INC_1,
                                           BS_INC_1, BS_INC_1, BS_INC_1,
                                           BS_INC_1, BS_INC_1 },
                    BS_VEC(uint16_t, 8)),
                BS_VEC(uint32_t, 8));
            BS_VAR_0[BS_INC_1 < 8 ? BS_INC_1 : 0] = BS_TEMP_68[0]
                * BS_TEMP_68[1] * BS_TEMP_68[2] * BS_TEMP_68[3] * BS_TEMP_68[4]
                * 6 * BS_TEMP_68[7];
        }
}
