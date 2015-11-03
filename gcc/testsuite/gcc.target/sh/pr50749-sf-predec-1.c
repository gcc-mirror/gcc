/* PR target/50749: Verify that pre-decrement addressing is generated.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmov.s\tfr\[0-9]\+,@-r\[0-9]\+" 1 } } */

float*
test_func_00 (float* p, float c)
{
  *--p = c;
  return p;
}

