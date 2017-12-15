/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2 -mfpu=fp-armv8 -march=armv8.2-a+fp16 -marm -mfloat-abi=hard" }  */

__fp16
test_select (__fp16 a, __fp16 b, __fp16 c)
{
  return (a < b) ? b : c;
}
/* { dg-final { scan-assembler "bpl" } } */
