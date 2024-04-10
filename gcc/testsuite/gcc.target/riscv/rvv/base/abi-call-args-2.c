/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O1" } */

#include <stdarg.h>
#include "riscv_vector.h"

int8_t
va_callee (int count, ...)
{
  size_t vlmax = __riscv_vsetvlmax_e8m1 ();
  vint8m1_t sum = __riscv_vmv_v_x_i8m1 (0, vlmax);
  va_list ap;
  va_start (ap, count);
  for (int i = count; i > 0; i--)
    {
      vint8m1_t arg = va_arg (ap, vint8m1_t);
      sum = __riscv_vredsum_vs_i8m1_i8m1 (arg, sum, vlmax);
    }
  va_end (ap);
  return __riscv_vmv_x_s_i8m1_i8 (sum);
}

/* Make sure the variadic arguments is not passed through the vector register.
 */
/* { dg-final { scan-assembler-not {vs[0-9]+r} } } */
/* { dg-final { scan-assembler-not {vsm} } } */
/* { dg-final { scan-assembler-not {vse[0-9]+} } } */
