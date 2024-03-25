/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
test_1 (vint32m1_t a) /* { dg-error {argument type 'vint32m1_t' requires the V ISA extension} } */
{
  return;
}
