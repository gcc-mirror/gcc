/* PR target/50749: Verify that subsequent post-increment addressings
   are generated.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmov.s\t@r\[0-9]\+\\+,fr\[0-9]\+" 5 { xfail *-*-*} } } */

float*
test_func_00 (float* p, float* x)
{
  float r = 0;
  r += *p++;
  r += *p++;
  *x = r;
  return p;
}

float*
test_func_01 (float* p, float* x)
{
  float r = 0;
  r += *p++;
  r += *p++;
  r += *p++;
  *x = r;
  return p;
}
