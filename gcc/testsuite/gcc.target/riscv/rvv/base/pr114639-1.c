/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include <riscv_vector.h>

extern size_t get_vl ();

vbool16_t
test (vuint64m4_t a)
{
  unsigned long b;
  return __riscv_vmsne_vx_u64m4_b16 (a, b, get_vl ());
}
