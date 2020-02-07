/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-final { scan-assembler-times "movq\[ \t\]*%rcx, .*" 1 } } */
/* { dg-final { scan-assembler-times "movq\[ \t\]*%rdx, .*" 1 } } */
/* { dg-final { scan-assembler-times "movq\[ \t\]*%r8, .*" 1 } } */
/* { dg-final { scan-assembler-times "movq\[ \t\]*%r9, .*" 1 } } */
/* { dg-final { scan-assembler-times "addsd\[ \t]*40\\\(%rsp\\\), .*" 1 } } */
/* { dg-final { scan-assembler-times "movq\[^\n\r\]*, %rax" 1 } } */

typedef struct
{
  double x;
} Double;

Double  __attribute__((ms_abi))
fn1 (Double x1, Double x2, Double x3, Double x4, Double x5)
{
  Double v;
  v.x = x1.x + x2.x + x3.x + x4.x + x5.x;
  return v;
}
