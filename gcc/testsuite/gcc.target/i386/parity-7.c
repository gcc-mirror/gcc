/* { dg-do compile } */
/* { dg-options "-O2 -march=core-avx2 -mno-popcnt" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-final { scan-assembler-times "test" 2 } } */
/* { dg-final { scan-assembler-not "shr" } } */

int foo(unsigned char x)
{
  return __builtin_parity(x);
}

int bar(unsigned char x)
{
  return __builtin_parityll(x);
}
