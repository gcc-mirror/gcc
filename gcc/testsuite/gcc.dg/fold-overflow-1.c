/* { dg-compile } */
/* { dg-skip-if "consts are shorts, not longs" { "m32c-*-*" } { "*" } { "" } } */
/* { dg-options "-O -ftrapping-math" } */

/* There should be exactly 2 +Inf in the assembly file.  */

float f1 =  __FLT_MAX__ + __FLT_MAX__;

float foo1(void)
{
  return  __FLT_MAX__ + __FLT_MAX__;
}

float f2 = 1.0f/0.0f;

float foo2(void)
{
  return 1.0f/0.0f;
}

/* { dg-final { scan-assembler-times "2139095040" 2 { target { ! mmix-*-* } } } } */
/* { dg-final { scan-assembler-times "#7f800000" 2 { target mmix-*-* } } } */
