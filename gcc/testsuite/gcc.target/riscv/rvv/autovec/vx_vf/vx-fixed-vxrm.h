#ifndef HAVE_DEFINED_VX_FIXED_VXRM_H
#define HAVE_DEFINED_VX_FIXED_VXRM_H

#include <riscv_vector.h>

int64_t go[VL] = {};
int64_t ga[VL] = {};

#define DEF_FIXED_BINARY_VX(VT, T, ES, SX, VXRM, FUNC)       \
void __attribute__((noinline))                               \
test_fixed_binary_##VT##_##VXRM##_##FUNC##_vx () {           \
  VT a = __riscv_vle##ES##_v_##SX##m1((T *)ga, VL);          \
  VT b;                                                      \
  T *bp = (T *)&b;                                           \
                                                             \
  for (int i = 0; i < VL; i++) {                             \
    bp[i] = 123;                                             \
  }                                                          \
                                                             \
  VT d = FUNC (a, b, VXRM, VL);                              \
                                                             \
  __riscv_vse##ES##_v_##SX##m1((T *)&go, d, VL);             \
}

#define DEF_FIXED_BINARY_VX_WRAP(VT, T, ES, SX, VXRM, FUNC) \
  DEF_FIXED_BINARY_VX(VT, T, ES, SX, VXRM, FUNC)

#endif
