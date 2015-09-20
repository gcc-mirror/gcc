/* PR target/50749: Verify that post-increment addressing is generated
   inside a loop.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "fmov.s\t@r\[0-9]\+\\+,fr\[0-9]\+" 3 { xfail *-*-*} } } */

float
test_func_00 (float* p, int c)
{
  float r = 0;
  do
  {
    r += *p++;
    r += *p++;
    r += *p++;
  } while (--c);
  return r;
}
