/* { dg-do run } */

typedef unsigned int u32;
#if __SIZEOF_INT128__
typedef unsigned long long u64;
typedef unsigned __int128 u128;
#else
typedef unsigned long u64;
typedef unsigned long long u128;
#endif

u128 g;

static __attribute__ ((noinline, noclone))
void check (u64 a, u64 b)
{
  if (a != 0 || b != 4)
    __builtin_abort ();
}

int
main (void)
{
  u64 d = (g ? 5 : 4);
  u32 f = __builtin_sub_overflow_p (d, (u128) d, (u64) 0);
  u128 x = g + f + d;
  check ((x >> 1) >> (sizeof (u64) * __CHAR_BIT__ - 1), x);
  return 0;
}
