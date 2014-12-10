/* PR tree-optimization/63302 */

#ifdef __SIZEOF_INT128__
#if __SIZEOF_INT128__ * __CHAR_BIT__ == 128
#define USE_INT128
#endif
#endif
#if __SIZEOF_LONG_LONG__ * __CHAR_BIT__ == 64
#define USE_LLONG
#endif

#ifdef USE_INT128
__attribute__((noinline, noclone)) int
foo (__int128 x)
{
  __int128 v = x & (((__int128) -1 << 63) | 0x7ff);
 
  return v == 0 || v == ((__int128) -1 << 63);
}
#endif

#ifdef USE_LLONG
__attribute__((noinline, noclone)) int
bar (long long x)
{
  long long v = x & (((long long) -1 << 31) | 0x7ff);
 
  return v == 0 || v == ((long long) -1 << 31);
}
#endif

int
main ()
{
#ifdef USE_INT128
  if (foo (0) != 1
      || foo (1) != 0
      || foo (0x800) != 1
      || foo (0x801) != 0
      || foo ((__int128) 1 << 63) != 0
      || foo ((__int128) -1 << 63) != 1
      || foo (((__int128) -1 << 63) | 1) != 0
      || foo (((__int128) -1 << 63) | 0x800) != 1
      || foo (((__int128) -1 << 63) | 0x801) != 0)
    __builtin_abort ();
#endif
#ifdef USE_LLONG
  if (bar (0) != 1
      || bar (1) != 0
      || bar (0x800) != 1
      || bar (0x801) != 0
      || bar (1LL << 31) != 0
      || bar (-1LL << 31) != 1
      || bar ((-1LL << 31) | 1) != 0
      || bar ((-1LL << 31) | 0x800) != 1
      || bar ((-1LL << 31) | 0x801) != 0)
    __builtin_abort ();
#endif
  return 0;
}
