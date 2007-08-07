/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target ilp32 } */

typedef unsigned int u32;
typedef unsigned long long u64;

u64 __attribute__((noinline))
foo (u32 high, u32 low)
{
  return ((u64)high << 32) | low;
}

u32 __attribute__((noinline))
right (u64 t)
{
  return (u32)(t >> 32);
}

u64 __attribute__((noinline))
left (u32 t)
{
  return (u64)t << 32;
}

u32 __attribute__((noinline))
right2 (u64 t)
{
  return (u32)(t >> 40);
}

u64 __attribute__((noinline))
left2 (u32 t)
{
  return (u64)t << 40;
}

u64 __attribute__((noinline))
zeroextend (u32 t)
{
  return (u64)t;
}

extern void abort ();

int
main ()
{
  if (foo (13000, 12000) != 55834574860000ULL)
    abort ();

  if (right (55834574860000ULL) != 13000)
    abort ();

  if (left (13000) != 55834574848000ULL)
    abort ();

  if (right2 (55834574860000ULL) != 50)
    abort ();

  if (left2 (13000) != 14293651161088000ULL)
    abort ();

  if (zeroextend (13000) != 13000ULL)
    abort ();

  return 0;
}
