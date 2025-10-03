/* { dg-do compile } */
/* { dg-additional-options "-w -march=rv64gcv -mabi=lp64d" { target rv64 } } */
/* { dg-additional-options "-w -march=rv32gcv -mabi=ilp32" { target rv32 } } */

#include <stdint-gcc.h>
#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
typedef int16_t int16;
typedef uint16_t uint16;
typedef int32_t int32;
typedef uint64_t uint64;
int32_t g_69, g_539;
int32_t *g_68;
void func_59(int32_t p_60) {
  BS_VEC(uint64, 2) BS_VAR_4;
  BS_VEC(int16, 8) BS_VAR_6;
  uint64 *LOCAL_CHECKSUM;
  int32_t *l_108 = &g_69;
  int64_t l_829 = 10;
  int32_t l_844 = -1;
  for (; g_69;) {
    int32_t l_924;
    if (p_60 * 2u) {
    BS_LABEL_0:
      *LOCAL_CHECKSUM ^= BS_VAR_4[3];
      for (l_924 = 3; l_924; l_924 -= 1) {
        BS_VEC(uint64, 8)
        BS_TEMP_600 = -__builtin_convertvector(BS_VAR_6, BS_VEC(uint64, 8));
        BS_VEC(uint64, 8)
        BS_TEMP_601 = __builtin_convertvector((BS_VEC(int32, 8)){p_60},
                                              BS_VEC(uint64, 8));
        BS_VAR_4[356358257141730375] =
            __builtin_convertvector(
                __builtin_shufflevector((BS_VEC(uint16, 2))0,
                                        (BS_VEC(uint16, 2))0, 1, 3, 0, 1, 2, 0,
                                        0, 2, 0, 0, 1, 2, 3, 3, 3, 2),
                BS_VEC(uint64, 16))[BS_VAR_6[4]] >
            (BS_VEC(uint64, 8)){0, BS_TEMP_600[1] ? BS_TEMP_601[1]
                                                  : 0}[l_829 != 0];
      }
    }
    if (*l_108)
      *g_68 |= g_539;
    __asm goto("" : : : : BS_LABEL_0);
    BS_VEC(int16, 4)
    BS_TEMP_681 = __builtin_shufflevector(
        (BS_VEC(int16, 2))__builtin_shufflevector(
            __builtin_convertvector(
                __builtin_shufflevector(BS_VAR_6, BS_VAR_6, 8, 6, 5, 8, 1, 3, 6,
                                        2, 0, 1, 2, 5, 8, 6, 5, 1, 5, 0, 3, 5,
                                        8, 2, 2, 4, 6, 0, 6, 4, 3, 3, 1, 2),
                BS_VEC(uint16, 32)),
            __builtin_convertvector((BS_VEC(int32, 32)){}, BS_VEC(uint16, 32)),
            42, 52) -
            __builtin_convertvector((BS_VEC(int32, 2)){l_844},
                                    BS_VEC(uint16, 2)) *
                ~0,
        ~(0 < __builtin_shufflevector(
                  __builtin_convertvector((BS_VEC(int32, 16)){p_60},
                                          BS_VEC(uint16, 16)),
                  (BS_VEC(uint16, 16)){20489, 3, 2, 4}, 19, 6)),
        1, 2, 0, 3);
    BS_VAR_6[0] =
        BS_TEMP_681[0] ^ BS_TEMP_681[1] ^ BS_TEMP_681[2] ^ BS_TEMP_681[3];
  }
}

