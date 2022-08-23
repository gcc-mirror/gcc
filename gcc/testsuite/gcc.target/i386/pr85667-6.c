/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-final { scan-assembler-times "movl\[^\n\r]*, %eax" 1 } } */
/* { dg-final { scan-assembler-times "flds\[^\n\r]*" 1 } } */
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

float __attribute__((ms_abi)) fn2 ()
{
  float v;
  v = 3.145;
  return v;
}
