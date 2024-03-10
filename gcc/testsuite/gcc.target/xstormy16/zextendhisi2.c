/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned long foo(unsigned short x)
{
  return x;
}
/* { dg-final { scan-assembler "mov r3,#0" } } */
