/* PR target/50749: Verify that pre-decrement addressing is generated.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-times "fmov.s\tfr\[0-9]\+,@-r\[0-9]\+" 1 } } */

float*
test_func_00 (float* p, float c)
{
  *--p = c;
  return p;
}

