/* { dg-do compile } */
/* { dg-options "-Os" } */
unsigned short foo(unsigned short x)
{
  return x*91;
}

/* { dg-final { scan-assembler "mul" } } */
