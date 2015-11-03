/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop3" } */

unsigned short
test1 (unsigned short a)
{
  a ^= 0x4002;
  a >>= 1;
  a |= 0x8000; /* Simplify to ((a >> 1) ^ 0xa001).  */
  return a;
}
/* { dg-final { scan-tree-dump "\\^ 40961" "forwprop3" } } */

unsigned short
test2 (unsigned short a)
{
  a |= 0x4002;
  a <<= 1;
  a ^= 0x0001; /* Simplify to ((a << 1) | 0x8005).  */
  return a;
}
/* { dg-final { scan-tree-dump "\\| 32773" "forwprop3" } } */

unsigned short
test3 (unsigned short a)
{
  a &= 0xd123;
  a ^= 0x6040;
  a |= 0xc031; /* Simplify to ((a & 0xd123) | 0xe071).  */
  return a;
}
/* { dg-final { scan-tree-dump "\\| 57457" "forwprop3" } } */

unsigned short
test4 (unsigned short a)
{
  a ^= 0x8002;
  a >>= 1;
  a |= 0x8000;
  return a;
}
/* { dg-final { scan-tree-dump "\\^ 49153" "forwprop3" } } */

unsigned short
test5 (unsigned short a)
{
  a ^= 0x8002;
  a >>= 1;
  a |= 0x8001; /* Only move shift inward: (((a >> 1) ^ 0x4001) | 0x8001).  */
  return a;
}
/* { dg-final { scan-tree-dump "\\^ 16385" "forwprop3" } } */
/* { dg-final { scan-tree-dump "\\| 32769" "forwprop3" } } */

short
test6 (short a)
{
  a &= 0x7fff;
  a >>= 2;
  return a;
}
/* { dg-final { scan-tree-dump "\\& 8191" "forwprop3" } } */

short
test7 (short a)
{
  a &= 0x8fff;
  a >>= 2;
  return a;
}
/* { dg-final { scan-tree-dump "\\& -7169" "forwprop3" } } */
