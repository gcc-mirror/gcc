/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

__attribute__((noipa)) int
t1 (int x)
{
  int y = x << 31;
  int z = y >> 31;
  return z;
}

__attribute__((noipa)) int
t2 (unsigned int x)
{
  int y = x << 31;
  int z = y >> 31;
  return z;
}

__attribute__((noipa)) int
t3 (int x)
{
  return (x << 31) >> 31;
}

__attribute__((noipa)) int
t4 (int x)
{
  return (x << 24) >> 24;
}

__attribute__((noipa)) int
t5 (int x)
{
  return (x << 16) >> 16;
}

__attribute__((noipa)) long long
t6 (long long x)
{
  return (x << 63) >> 63;
}

__attribute__((noipa)) long long
t7 (long long x)
{
  return (x << 56) >> 56;
}

__attribute__((noipa)) long long
t8 (long long x)
{
  return (x << 48) >> 48;
}

__attribute__((noipa)) long long
t9 (long long x)
{
  return (x << 32) >> 32;
}

/* { dg-final { scan-tree-dump-not " >> " "optimized" } } */
/* { dg-final { scan-tree-dump-not " << " "optimized" } } */
