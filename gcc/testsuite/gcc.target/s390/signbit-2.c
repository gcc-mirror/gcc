/* { dg-do run } */
/* { dg-options "-O2 -march=z9-ec -mzarch -save-temps" } */
/* { dg-final { scan-assembler-times {\ttdcet\t} 2 } } */
/* { dg-final { scan-assembler-times {\ttdcdt\t} 2 } } */
/* { dg-final { scan-assembler-times {\ttdcxt\t} 2 } } */

/* Decimal Floating-Point */

__attribute__ ((noipa))
int signbit_dec32_reg (_Decimal32 x) { return __builtin_signbit (x); }
__attribute__ ((noipa))
int signbit_dec32_mem (_Decimal32 *x) { return __builtin_signbit (*x); }
__attribute__ ((noipa))
int signbit_dec64_reg (_Decimal64 x) { return __builtin_signbit (x); }
__attribute__ ((noipa))
int signbit_dec64_mem (_Decimal64 *x) { return __builtin_signbit (*x); }

__attribute__ ((noipa))
int
signbit_dec128_reg (_Decimal128 x)
{
  __asm__ ("" : "+f" (x));
  return __builtin_signbit (x);
}

__attribute__ ((noipa))
int signbit_dec128_mem (_Decimal128 *x) { return __builtin_signbit (*x); }

#include "signbit.h"
TEST (dec32, _Decimal32, __builtin_infd32(), __builtin_nand32("42"), 0.df, 42.df)
TEST (dec64, _Decimal64, __builtin_infd64(), __builtin_nand64("42"), 0.dd, 42.dd)
TEST (dec128, _Decimal128, __builtin_infd128(), __builtin_nand128("42"), 0.dl, 42.dl)

int
main (void)
{
  test_dec32 ();
  test_dec64 ();
  test_dec128 ();
}
