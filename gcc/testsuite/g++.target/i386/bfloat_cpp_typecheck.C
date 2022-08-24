/* { dg-do compile } */
/* { dg-options "-msse2 -O3 --save-temps" } */

void foo (void)
{
  __bf16 (); /* { dg-bogus {invalid conversion to type '__bf16'} } */
  __bf16 a = __bf16(); /* { dg-bogus {invalid conversion to type '__bf16'} } */
  __bf16 (0x1234); /* { dg-error {invalid conversion to type '__bf16'} } */
  __bf16 (0.1); /* { dg-error {invalid conversion to type '__bf16'} } */
}
