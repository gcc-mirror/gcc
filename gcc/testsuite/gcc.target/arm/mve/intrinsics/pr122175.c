/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

#include <arm_mve.h>

#ifdef __cplusplus
extern "C" {
#endif

uint16x8_t test_1(uint16x8_t v11) {
  return vbicq_n_u16(v11, 0x8000);
}

uint16x8_t test_2(uint16x8_t v11) {
  return vorrq_n_u16(v11, 0x8000);
}

uint16x8_t test_3() {
  return vmvnq_n_u16(0x8000);
}

mve_pred16_t pred;
uint16x8_t test_4(uint16x8_t v11) {
  return vbicq_m_n_u16(v11, 0x8000, pred);
}

uint16x8_t test_5(uint16x8_t v11) {
  return vorrq_m_n_u16(v11, 0x8000, pred);
}

uint16x8_t test_6() {
  return vmvnq_x_n_u16(0x8000, pred);
}

#ifdef __cplusplus
}
#endif
