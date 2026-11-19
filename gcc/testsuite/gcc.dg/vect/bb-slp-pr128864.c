/* { dg-do compile } */

int
f (unsigned char a, unsigned char b)
{
  asm goto ("" : "=r" (a), "=r" (b): : : l1);
  return a * 8 + b * 4;
l1:
  return a * 2 + b * 5;
}
