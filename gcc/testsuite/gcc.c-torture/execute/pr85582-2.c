/* PR target/85582 */

#ifdef __SIZEOF_INT128__
typedef __int128 S;
typedef unsigned __int128 U;
#else
typedef long long S;
typedef unsigned long long U;
#endif

__attribute__((noipa)) S
f1 (S x, int y)
{
  x = x << (y & 5);
  x += y;
  return x;
}

__attribute__((noipa)) S
f2 (S x, int y)
{
  x = x >> (y & 5);
  x += y;
  return x;
}

__attribute__((noipa)) U
f3 (U x, int y)
{
  x = x >> (y & 5);
  x += y;
  return x;
}

int
main ()
{
  S a = (S) 1 << (sizeof (S) * __CHAR_BIT__ - 7);
  S b = f1 (a, 12);
  if (b != ((S) 1 << (sizeof (S) * __CHAR_BIT__ - 3)) + 12)
    __builtin_abort ();
  S c = (U) 1 << (sizeof (S) * __CHAR_BIT__ - 1);
  S d = f2 (c, 12);
  if ((U) d != ((U) 0x1f << (sizeof (S) * __CHAR_BIT__ - 5)) + 12)
    __builtin_abort ();
  U e = (U) 1 << (sizeof (U) * __CHAR_BIT__ - 1);
  U f = f3 (c, 12);
  if (f != ((U) 1 << (sizeof (U) * __CHAR_BIT__ - 5)) + 12)
    __builtin_abort ();
  return 0;
}
