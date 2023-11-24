/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v3" } */
/* { dg-final { scan-assembler "global\t__divdi3" } } */
/* { dg-final { scan-assembler "global\t__moddi3" } } */

int
foo (unsigned int len)
{
  return ((long)len) * 234 / 5;
}

int
bar (unsigned int len)
{
  return ((long)len) * 234 % 5;
}
