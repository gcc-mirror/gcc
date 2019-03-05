/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "movl\[^\n\r]*, %eax|mov\[ \t]*eax," 1 } } */

typedef struct
{
  float x;
} Float;

Float __attribute__((ms_abi)) fn1 ()
{
  Float v;
  v.x = 3.145;
  return v;
}
