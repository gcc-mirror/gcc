/* { dg-do run } */
/* { dg-options "-O2" } */

extern
#ifdef __cplusplus
"C"
#endif
void abort (void);

#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ == 64
__attribute__((noinline, noclone))
unsigned long long
f1 (unsigned long long x, unsigned int y)
{
  return (x << y) | (x >> ((-y) & 63));
}

__attribute__((noinline, noclone))
unsigned long long
f2 (unsigned long long x, unsigned int y)
{
  return (x << y) + (x >> ((-y) & 63));
}

__attribute__((noinline, noclone))
unsigned long long
f3 (unsigned long long x, unsigned int y)
{
  return (x << y) ^ (x >> ((-y) & 63));
}

#if __CHAR_BIT__ * __SIZEOF_INT128__ == 128
__attribute__((noinline, noclone))
unsigned __int128
f4 (unsigned __int128 x, unsigned int y)
{
  return (x << y) | (x >> ((-y) & 127));
}

__attribute__((noinline, noclone))
unsigned __int128
f5 (unsigned __int128 x, unsigned int y)
{
  return (x << y) + (x >> ((-y) & 127));
}

__attribute__((noinline, noclone))
unsigned __int128
f6 (unsigned __int128 x, unsigned int y)
{
  return (x << y) ^ (x >> ((-y) & 127));
}
#endif
#endif

int
main ()
{
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ == 64
  if (f1 (0x123456789abcdef0ULL, 0) != 0x123456789abcdef0ULL)
    abort ();
  if (f2 (0x123456789abcdef0ULL, 0) != 0x2468acf13579bde0ULL)
    abort ();
  if (f3 (0x123456789abcdef0ULL, 0) != 0)
    abort ();
  if (f1 (0x123456789abcdef0ULL, 1) != 0x2468acf13579bde0ULL)
    abort ();
  if (f2 (0x123456789abcdef0ULL, 1) != 0x2468acf13579bde0ULL)
    abort ();
  if (f3 (0x123456789abcdef0ULL, 1) != 0x2468acf13579bde0ULL)
    abort ();
#if __CHAR_BIT__ * __SIZEOF_INT128__ == 128
  if (f4 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 0)
      != ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
          | 0x0fedcba987654321ULL))
    abort ();
  if (f5 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 0)
      != ((((unsigned __int128) 0x2468acf13579bde0ULL) << 64)
          | 0x1fdb97530eca8642ULL))
    abort ();
  if (f6 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 0) != 0)
    abort ();
  if (f4 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 1)
      != ((((unsigned __int128) 0x2468acf13579bde0ULL) << 64)
          | 0x1fdb97530eca8642ULL))
    abort ();
  if (f5 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 1)
      != ((((unsigned __int128) 0x2468acf13579bde0ULL) << 64)
          | 0x1fdb97530eca8642ULL))
    abort ();
  if (f6 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 1)
      != ((((unsigned __int128) 0x2468acf13579bde0ULL) << 64)
          | 0x1fdb97530eca8642ULL))
    abort ();
#endif
#endif
  return 0;
}
