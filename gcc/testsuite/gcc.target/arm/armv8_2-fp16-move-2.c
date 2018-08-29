/* { dg-do compile }  */
/* { dg-require-effective-target arm_v8_2a_fp16_scalar_ok }  */
/* { dg-options "-O2 -marm" }  */
/* { dg-add-options arm_v8_2a_fp16_scalar }  */

__fp16
test_select (__fp16 a, __fp16 b, __fp16 c)
{
  return (a < b) ? b : c;
}
/* { dg-final { scan-assembler "bmi" } } */
