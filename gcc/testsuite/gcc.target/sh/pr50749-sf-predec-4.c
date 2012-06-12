/* PR target/50749: Verify that pre-decrement addressing is generated
   inside a loop.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2*" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-times "fmov.s\tfr\[0-9]\+,@-r\[0-9]\+" 3 { xfail *-*-*} } } */

float*
test_func_00 (float* p, int c, float x)
{
  do
  {
    *--p = x;
    *--p = x;
    *--p = x;
  } while (--c);
  return p;
}
