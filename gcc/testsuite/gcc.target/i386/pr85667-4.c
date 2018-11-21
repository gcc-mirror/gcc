/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "movq\[^\n\r]*, %rax|mov\[ \t]*rax," 1 } } */

typedef struct
{
  double x;
} Double;

Double  __attribute__((ms_abi)) fn1 ()
{
  Double v;
  v.x = 3.145;
  return v;
}
