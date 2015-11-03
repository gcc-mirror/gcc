/* PR target/50749: Verify that subsequent pre-decrement addressings
   are generated.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmov.s\tfr\[0-9]\+,@-r\[0-9]\+" 5 { xfail *-*-*} } } */

float*
test_func_00 (float* p, float c)
{
  *--p = c;
  *--p = c;
  return p;
}

float*
test_func_01 (float* p, float c)
{
  *--p = c;
  *--p = c;
  *--p = c;
  return p;
}
