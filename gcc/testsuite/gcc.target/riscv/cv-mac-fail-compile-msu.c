/* { dg-do compile } */
/* { dg-require-effective-target cv_mac } */
/* { dg-options "-march=rv32i_xcvmac -mabi=ilp32" } */

#include <stdint.h>

extern int32_t res1;
extern int32_t res2;
extern int32_t res3;
extern int32_t res4;
extern int32_t res5;
extern int32_t res6;

int
main (void)
{
  res1 = __builtin_riscv_cv_mac_msu (12147483649, 21, 47); /* { dg-warning "overflow in conversion" } */
  res2 = __builtin_riscv_cv_mac_msu (648, 12147483649, 48); /* { dg-warning "overflow in conversion" } */
  res3 = __builtin_riscv_cv_mac_msu (648, 48, 12147483649); /* { dg-warning "overflow in conversion" } */
  res4 = __builtin_riscv_cv_mac_msu (-2147483649, 21, 47); /* { dg-warning "overflow in conversion" } */
  res5 = __builtin_riscv_cv_mac_msu (648, -2147483649, 48); /* { dg-warning "overflow in conversion" } */
  res6 = __builtin_riscv_cv_mac_msu (648, 48, -2147483649); /* { dg-warning "overflow in conversion" } */

  return res1+res2+res3+res4+res5+res6;
}
