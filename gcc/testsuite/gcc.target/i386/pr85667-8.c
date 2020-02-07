/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-times "movd\[ \t\]*%ecx, .*" 1 } } */
/* { dg-final { scan-assembler-times "movd\[ \t\]*%edx, .*" 1 } } */
/* { dg-final { scan-assembler-times "movd\[ \t\]*%r8d, .*" 1 } } */
/* { dg-final { scan-assembler-times "movd\[ \t\]*%r9d, .*" 1 } } */
/* { dg-final { scan-assembler-times "addss\[ \t]*40\\\(%rsp\\\), .*" 1 } } */
/* { dg-final { scan-assembler-times "movd\[^\n\r\]*, %eax" 1 } } */

typedef struct
{
  float x;
} Float;

Float  __attribute__((ms_abi))
fn1 (Float x1, Float x2, Float x3, Float x4, Float x5)
{
  Float v;
  v.x = x1.x + x2.x + x3.x + x4.x + x5.x;
  return v;
}
