/* PR target/50749: Verify that post-increment addressing is generated.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmov.s\t@r\[0-9]\+\\+,fr\[0-9]\+" 1 } } */

float*
test_func_00 (float* p, float* x)
{
  float r = 0;
  r += *p++;
  *x = r;
  return p;
}
