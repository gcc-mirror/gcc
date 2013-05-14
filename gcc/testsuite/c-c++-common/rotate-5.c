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

#if __CHAR_BIT__ * __SIZEOF_INT128__ == 128
__attribute__((noinline, noclone))
unsigned __int128
f2 (unsigned __int128 x, unsigned int y)
{
  return (x << y) | (x >> ((-y) & 128));
}
#endif
#endif

int
main ()
{
#if __CHAR_BIT__ * __SIZEOF_LONG_LONG__ == 64
  if (f1 (0x123456789abcdef0ULL, 0) != 0x123456789abcdef0ULL)
    abort ();
#if __CHAR_BIT__ * __SIZEOF_INT128__ == 128
  if (f2 ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
	  | 0x0fedcba987654321ULL, 0)
      != ((((unsigned __int128) 0x123456789abcdef0ULL) << 64)
          | 0x0fedcba987654321ULL))
    abort ();
#endif
#endif
  return 0;
}
