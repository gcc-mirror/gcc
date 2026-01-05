/* { dg-do compile } */
/* { dg-options "-O3 -w -Wno-psabi" } */

#include <stdint.h>
#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
#define BITCAST(T, F, arg)                                                     \
    (union {                                                                   \
        F src;                                                                 \
        T dst                                                                  \
    }){ arg }                                                                  \
        .dst
#include <immintrin.h>
#define BARRIER_v8u8(x) (BS_VEC(uint8_t, 8)) _mm_avg_pu8((__m64)x, (__m64)x)
uint64_t BS_CHECKSUM_ARR[];
BS_VEC(int8_t, 16) backsmith_pure_2(BS_VEC(int32_t, 8));
BS_VEC(uint16_t, 4)
backsmith_pure_0(BS_VEC(uint8_t, 4), int16_t, int64_t BS_ARG_2)
{
    int8_t BS_VAR_1;
    BS_VEC(uint8_t, 16) BS_VAR_4[7];
    if (BS_ARG_2)
    {
        if ((uint16_t)BS_ARG_2)
            for (uint16_t BS_INC_0;;)
                __builtin_convertvector(
                    __builtin_shufflevector(BS_VAR_4[BS_INC_0],
                                            BS_VAR_4[BS_INC_0], 8, 4, 5, 8),
                    BS_VEC(uint16_t, 4));
        BS_VAR_1 = __builtin_convertvector(
            __builtin_shufflevector((BS_VEC(uint8_t, 8)){}, BARRIER_v8u8({}), 4,
                                    10, 2, 1),
            BS_VEC(int8_t, 4))[BS_ARG_2];
        uint16_t BS_TEMP_11 = BS_VAR_1;
        return (BS_VEC(uint16_t, 4)){ BS_TEMP_11 };
    }
    __builtin_convertvector(__builtin_shufflevector((BS_VEC(uint8_t, 8)){},
                                                    (BS_VEC(uint8_t, 8)){}, 2,
                                                    6, 4, 4),
                            BS_VEC(uint16_t, 4));
}
static int32_t *func_31(int32_t *, uint64_t);
uint16_t func_1()
{
    BITCAST(
        int64_t, BS_VEC(int16_t, 4),
        (__builtin_convertvector(
             __builtin_shufflevector((BS_VEC(uint32_t, 4)){},
                                     (BS_VEC(uint32_t, 4)){}, 0, 3, 3, 0, 2, 7,
                                     2, 5, 3, 5, 0, 4, 0, 1, 1, 7, 1, 0, 6, 7,
                                     6, 3, 4, 6, 3, 3, 1, 7, 3, 6, 0, 0),
             BS_VEC(int16_t, 32)),
         __builtin_convertvector(
             __builtin_shufflevector(
                 backsmith_pure_0((BS_VEC(uint8_t, 4)){}, 0,
                                  BITCAST(int64_t, BS_VEC(int32_t, 2), )),
                 backsmith_pure_0(
                     (BS_VEC(uint8_t, 4)){}, 0,
                     BITCAST(int64_t, BS_VEC(int32_t, 2),
                             __builtin_convertvector(
                                 __builtin_shufflevector(
                                     backsmith_pure_2(__builtin_convertvector(
                                         (BS_VEC(int64_t, 8)){},
                                         BS_VEC(int32_t, 8))),
                                     backsmith_pure_2(__builtin_convertvector(
                                         (BS_VEC(int64_t, 8)){},
                                         BS_VEC(int32_t, 8))),
                                     9, 2),
                                 BS_VEC(int32_t, 2)))),
                 3, 7, 5, 5, 1, 0, 2, 1, 1, 4, 6, 4, 5, 1, 5, 6, 1, 0, 1, 6, 4,
                 1, 2, 3, 1, 1, 1, 0, 7, 2, 5, 1),
             BS_VEC(int16_t, 32)),
         1));
    int32_t l_969;
    int8_t l_1016 = 1;
    func_31(&l_969, l_1016);
    __builtin_convertvector((BS_VEC(int32_t, 32)){}, BS_VEC(int16_t, 32)),
        __builtin_convertvector((BS_VEC(int32_t, 2)){}, BS_VEC(uint8_t, 2));
    int l_572 = 2558744285;
    func_31(0, l_572);
}
int32_t *func_31(int32_t *, uint64_t p_33)
{
    uint64_t LOCAL_CHECKSUM;
    backsmith_pure_0(
        __builtin_convertvector((BS_VEC(int32_t, 4)){}, BS_VEC(uint8_t, 4)),
        20966, p_33);
    for (uint32_t BS_TEMP_215; BS_TEMP_215;)
        BS_CHECKSUM_ARR[6] += LOCAL_CHECKSUM;
}
