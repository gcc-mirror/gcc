/* { dg-options "" } */
/* { dg-do compile } */
/* { dg-add-options riscv_v } */
/* PR target/122270 */

#include "riscv_vector.h"

void a(vfloat32m1_t b, vfloat32m1x4_t *c) {
  *c = __riscv_vset_v_f32m1_f32m1x4(*c, 3, b);
}
