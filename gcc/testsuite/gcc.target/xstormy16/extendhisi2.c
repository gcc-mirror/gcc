/* { dg-do compile } */
/* { dg-options "-O2" } */
long foo(short x)
{
  return x;
}
/* { dg-final { scan-assembler "asr r3,#15" } } */
