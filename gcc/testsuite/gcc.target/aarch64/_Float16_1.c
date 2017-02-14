/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+nofp16" } */

#pragma GCC target ("arch=armv8.2-a+nofp16")

_Float16
foo_v8 (_Float16 x, _Float16 y, unsigned int *eval)
{
  *eval = __FLT_EVAL_METHOD__;
  return x * x + y;
}

__fp16
bar_v8 (__fp16 x, __fp16 y, unsigned int *eval)
{
  *eval = __FLT_EVAL_METHOD__;
  return x * x + y;
}

#pragma GCC target ("arch=armv8.2-a+fp16")

_Float16
foo_v82 (_Float16 x, _Float16 y, unsigned int *eval)
{
  *eval = __FLT_EVAL_METHOD__;
  return x * x + y;
}

__fp16
bar_v82 (__fp16 x, __fp16 y, unsigned int *eval)
{
  *eval = __FLT_EVAL_METHOD__;
  return x * x + y;
}

/* Test that we merge to FMA operations.  This indicates that we are not
   making extraneous conversions between modes.  */

/* Three FMA operations in 32-bit precision, from foo_v8, bar_v8, bar_v82.  */
/* { dg-final { scan-assembler-times "fmadd\ts\[0-9\]\+" 3 } } */

/* One FMA operation in 16-bit precision, from foo_v82.  */
/* { dg-final { scan-assembler-times "fmadd\th\[0-9\]\+" 1 } } */

/* Test that we are resetting the __FLT_EVAL_METHOD__.  */
/* { dg-final { scan-assembler-times "mov\tw\[0-9\]\+, 16" 2 } } */
/* { dg-final { scan-assembler-times "str\twzr" 2 } } */
