/* { dg-do run { target int128 } } */
/* { dg-additional-options "-fgcse-after-reload" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
int a, b, c;
__int128 e;
int
d (u16 g)
{
  u64 f = __builtin_bswap64 (c);
  f = g == a;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  __builtin_memmove (&f, &e, 1);
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  __builtin_memmove ((char *) &f + sizeof (f) - 1,
		     (char *) &e + sizeof (e) - 1, 1);
#elif __BYTE_ORDER__ == __ORDER_PDP_ENDIAN__
  __builtin_memmove ((char *) &f + sizeof (f) - 2,
		     (char *) &e + sizeof (e) - 2, 1);
#else
#error "endian unknown?"
#endif
  e >>= b;
  return a + f;
}

int
main (void)
{
  __int128 x = d (0);
  if (x != 0)
    __builtin_abort ();
  return 0;
}
