/* { dg-compile } */
/* { dg-options "-O -ftrapping-math" } */

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

/* { dg-final { scan-assembler-times "2139095040" 2 } } */
