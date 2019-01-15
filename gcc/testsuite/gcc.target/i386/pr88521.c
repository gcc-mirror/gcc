/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "movl\[^\n\r]*, %eax|mov\[ \t]*eax," 1 } } */
/* { dg-final { scan-assembler-times "movss\[^\n\r]*, %xmm" 1 } } */
/* { dg-final { scan-assembler-times "movsd\[^\n\r]*, %xmm" 1 } } */
typedef struct
{
  float x;
} Float;

Float  __attribute__((ms_abi)) fn1()
{
  Float v;
  v.x = 3.145F;
  return v;
}

float  __attribute__((ms_abi))  fn2 ()
{
  float v;
  v = 3.145F;
  return v;
}

double  __attribute__((ms_abi))  fn3 ()
{
  double v;
  v = 3.145;
  return v;
}
