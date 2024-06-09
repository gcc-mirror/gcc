#include "riscv_vector.h"

typedef _Float16 float16_t;
typedef float float32_t;
typedef double float64_t;

vfloat64m4x2_t test_vloxseg2ei16_v_f64m4x2(const float64_t *rs1,
                                           vuint16m1_t rs2, size_t vl) {
  return __riscv_vloxseg2ei16(rs1, rs2, vl);
}

vfloat64m4x2_t test_vloxseg2ei16_v_f64m4x2_m(vbool16_t vm, const float64_t *rs1,
                                             vuint16m1_t rs2, size_t vl) {
  return __riscv_vloxseg2ei16(vm, rs1, rs2, vl);
}

vfloat64m4x2_t test_vloxseg2ei16_v_f64m4x2_tum(vbool16_t vm, vfloat64m4x2_t vd,
                                               const float64_t *rs1,
                                               vuint16m1_t rs2, size_t vl) {
  return __riscv_vloxseg2ei16_tum(vm, vd, rs1, rs2, vl);
}

vfloat64m4x2_t test_vloxseg2ei16_v_f64m4x2_tumu(vbool16_t vm, vfloat64m4x2_t vd,
                                                const float64_t *rs1,
                                                vuint16m1_t rs2, size_t vl) {
  return __riscv_vloxseg2ei16_tumu(vm, vd, rs1, rs2, vl);
}

vfloat64m4x2_t test_vloxseg2ei16_v_f64m4x2_mu(vbool16_t vm, vfloat64m4x2_t vd,
                                              const float64_t *rs1,
                                              vuint16m1_t rs2, size_t vl) {
  return __riscv_vloxseg2ei16_mu(vm, vd, rs1, rs2, vl);
}

vfloat64m4x2_t test_vloxseg2ei16_v_f64m4x2_tu(vfloat64m4x2_t vd,
                                              const float64_t *rs1,
                                              vuint16m1_t rs2, size_t vl) {
  return __riscv_vloxseg2ei16_tu(vd, rs1, rs2, vl);
}
