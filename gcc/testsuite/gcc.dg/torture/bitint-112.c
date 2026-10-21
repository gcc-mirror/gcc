/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* Counts of _BitInt not wider than MAX_FIXED_MODE_SIZE are expanded like
   counts of other integers, also with a value at zero that the target
   does not produce.  */

#if __BITINT_MAXWIDTH__ >= 64
typedef unsigned _BitInt(64) U64;
typedef signed _BitInt(64) S64;
typedef unsigned _BitInt(32) U32;

static const unsigned char tab64[64] = {
  0, 1, 48, 2, 57, 49, 28, 3, 61, 58, 50, 42, 38, 29, 17, 4,
  62, 55, 59, 36, 53, 51, 43, 22, 45, 39, 33, 30, 24, 18, 12, 5,
  63, 47, 56, 27, 60, 41, 37, 16, 54, 35, 52, 21, 44, 32, 23, 11,
  46, 26, 40, 15, 34, 20, 31, 10, 25, 14, 19, 9, 13, 8, 7, 6 };

static const unsigned char clztab[32] = {
  0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
  8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31 };

[[gnu::noipa]] int
f (U64 x)
{
  return x ? tab64[((x & -x) * 0x03f79d71b4cb0a89uwb) >> 58] : 32;
}

/* The test is on the signed value, the count on the unsigned one.  */
[[gnu::noipa]] int
g (S64 x)
{
  U64 u = x;
  return x ? tab64[((u & -u) * 0x03f79d71b4cb0a89uwb) >> 58] : 32;
}

[[gnu::noipa]] int
h (U32 x)
{
  U32 v = x;
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  return x ? 31 - clztab[(v * 0x07C4ACDDuwb) >> 27] : 5;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 64
  if (f (0) != 32 || f (8) != 3 || f ((U64) 1 << 63) != 63)
    __builtin_abort ();
  if (g (0) != 32 || g (-4) != 2)
    __builtin_abort ();
  if (h (0) != 5 || h (1) != 31 || h ((U32) 1 << 31) != 0)
    __builtin_abort ();
#endif
  return 0;
}
