/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int foo_plus(unsigned int a, unsigned int b)
{
  return (a << 2) + (b << 2);
}

unsigned int foo_and(unsigned int a, unsigned int b)
{
  return (a << 2) & (b << 2);
}

unsigned int foo_ior(unsigned int a, unsigned int b)
{
  return (a << 2) | (b << 2);
}

unsigned int foo_xor(unsigned int a, unsigned int b)
{
  return (a << 2) ^ (b << 2);
}

unsigned int bar_and(unsigned int a, unsigned int b)
{
  return (a >> 2) & (b >> 2);
}

unsigned int bar_ior(unsigned int a, unsigned int b)
{
  return (a >> 2) | (b >> 2);
}

unsigned int bar_xor(unsigned int a, unsigned int b)
{
  return (a >> 2) ^ (b >> 2);
}

int baz_and(int a, int b)
{
  return (a >> 2) & (b >> 2);
}

int baz_ior(int a, int b)
{
  return (a >> 2) | (b >> 2);
}

int baz_xor(int a, int b)
{
  return (a >> 2) ^ (b >> 2);
}

/* { dg-final { scan-tree-dump-times " << " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " >> " 6 "optimized" } } */

