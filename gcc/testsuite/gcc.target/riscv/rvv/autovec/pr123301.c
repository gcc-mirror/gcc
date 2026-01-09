/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
#define BITCAST(T, F, arg)                                                     \
    ((union {                                                                  \
        F src;                                                                 \
        T dst;                                                                 \
    })arg)                                                                     \
        .dst
#include <riscv_bitmanip.h>
BS_VEC(uint64_t, 2)
backsmith_snippet_423(BS_VEC(int16_t, 2), BS_VEC(int32_t, 8), uint8_t)
{}
uint32_t backsmith_pure_1(BS_VEC(uint32_t, 2) BS_ARG_2, int8_t BS_ARG_3)
{
    BS_VEC(uint64_t, 4) BS_VAR_0;
    int32_t BS_VAR_4;
    uint64_t BS_TEMP_105 = 8;
    for (uint64_t BS_INC_0 = 0; BS_INC_0 < BS_TEMP_105; BS_INC_0 += 1)
        if (BS_ARG_2[1])
        {
            BS_VAR_4 = BS_INC_0;
            BS_VEC(uint32_t, 2)
            BS_TEMP_107 = __builtin_convertvector(
                (BS_VEC(int32_t, 2)){ BS_VAR_4, BS_VAR_4 },
                BS_VEC(uint32_t, 2));
            BS_VEC(uint32_t, 2)
            BS_TEMP_108 = __builtin_convertvector(
                (BS_VEC(int8_t, 2)){ BS_ARG_3 }, BS_VEC(uint32_t, 2));
            if (BITCAST(uint64_t, BS_VEC(uint32_t, 2),
                        ((BS_VEC(uint32_t, 2)){
                            BS_TEMP_107[0] ? BS_TEMP_108[0] : 0,
                            BS_TEMP_107[1] ? BS_TEMP_108[1] : 0 }))
                < backsmith_snippet_423(
                    __builtin_convertvector((BS_VEC(uint64_t, 2)){},
                                            BS_VEC(int16_t, 2)),
                    (BS_VEC(int32_t, 8)){}, 0)[1])
                BS_VAR_0 |= __builtin_convertvector(
                    (BS_VEC(int32_t, 4)){ BS_VAR_4 }, BS_VEC(uint64_t, 4));
        }
    if (BS_VAR_0[0])
        for (;;)
            ;
}
