#ifndef HAVE_DEFINED_PR123317_RUN_H
#define HAVE_DEFINED_PR123317_RUN_H

#include <stdint.h>

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
#define safe_mod_func_uint32_t_u_u(_ui1, _ui2)                                 \
  ({                                                                           \
    uint32_t ui1 = _ui1;                                                       \
    uint32_t ui2 = _ui2;                                                       \
    ui2 == 0 ? ui1 : 0;                                                        \
  })

uint64_t BS_CHECKSUM;
int32_t g_1577;

void func_5(int32_t p_8, int32_t p_9, uint64_t p_10) {
  BS_VEC(uint64_t, 4) BS_VAR_1[9] = {};
  uint64_t LOCAL_CHECKSUM = 0;
  int32_t l_1981 = 0;

  for (p_10 = 0; p_10 <= 4; p_10 += g_1577 -= 1) {
    l_1981 ^= safe_mod_func_uint32_t_u_u(p_8, p_9);
    BS_VAR_1[0] OP (BS_VEC(uint64_t, 4)){l_1981, l_1981, l_1981, l_1981};
  }

  for (uint32_t BS_TEMP_121 = 0; BS_TEMP_121 < 9; BS_TEMP_121++)
    for (uint32_t BS_TEMP_122 = 0; BS_TEMP_122 < 4; BS_TEMP_122++)
      LOCAL_CHECKSUM ^=
          BS_VAR_1[BS_TEMP_121][BS_TEMP_122] + LOCAL_CHECKSUM >> 2;

  BS_CHECKSUM += LOCAL_CHECKSUM;
}

void func_1() {
  int16_t l_32 = (int16_t)42122;
  func_5(l_32, 0, l_32);
}

int main() {
  func_1();

  if (BS_CHECKSUM != EXPECT)
    __builtin_abort();

  return 0;
}

#endif
