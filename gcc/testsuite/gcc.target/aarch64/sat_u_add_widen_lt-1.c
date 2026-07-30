/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned long long u64;

/* Written with < rather than <=, so the compare holds MAX - 1.  The sum
   equal to MAX takes the other arm, which is MAX as well.  */

u32
lt32 (u32 a, u32 b)
{
  u64 t = (u64) a + b;
  return (u32) (t < 0xffffffffull ? t : 0xffffffffull);
}

u32
le32 (u32 a, u32 b)
{
  u64 t = (u64) a + b;
  return (u32) (t <= 0xffffffffull ? t : 0xffffffffull);
}

u16
lt16 (u16 a, u16 b)
{
  u32 t = (u32) a + b;
  return (u16) (t < 0xffffu ? t : 0xffffu);
}

/* { dg-final { scan-tree-dump-times "\\.SAT_ADD " 3 "optimized" } } */
