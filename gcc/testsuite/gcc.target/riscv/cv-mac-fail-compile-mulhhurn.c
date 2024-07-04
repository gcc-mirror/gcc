/* { dg-do compile } */
/* { dg-require-effective-target cv_mac } */
/* { dg-options "-march=rv32i_xcvmac -mabi=ilp32" } */
/* { dg-skip-if "Skip LTO tests of builtin compilation" { *-*-* } { "-flto" } } */

#include <stdint.h>

extern uint32_t res1;
extern uint32_t res2;
extern uint32_t res3;
extern uint32_t res4;
extern uint32_t res5;

int
main (void)
{
  res1 = __builtin_riscv_cv_mac_mulhhuRN (648, 219, -1); /* { dg-error "invalid argument to built-in function" "" { target *-*-* } } */
  res2 = __builtin_riscv_cv_mac_mulhhuRN (648, 219, 0);
  res3 = __builtin_riscv_cv_mac_mulhhuRN (648, 219, 15);
  res4 = __builtin_riscv_cv_mac_mulhhuRN (648, 219, 31);
  res5 = __builtin_riscv_cv_mac_mulhhuRN (648, 219, 32); /* { dg-error "invalid argument to built-in function" "" { target *-*-* } } */

  return res1+res2+res3+res4+res5;
}
