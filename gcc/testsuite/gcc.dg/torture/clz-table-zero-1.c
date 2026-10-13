/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

/* Table-based count-leading-zeros (the zstd De Bruijn fallback) guarded
   against zero by the caller.  forwprop turned the lookup into a call with
   an explicit value at zero even when the target does not define one, and
   the guard was then folded away.  */

static const unsigned T[32] = {
  0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
  8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31
};

static inline unsigned
clz_fb (unsigned v)
{
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  return 31 - T[(v * 0x07C4ACDDU) >> 27];
}

__attribute__((noipa)) unsigned
f (unsigned v)
{
  return v ? clz_fb (v) : 32;
}

__attribute__((noipa)) unsigned
g (unsigned v)
{
  unsigned x = v;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  unsigned r = 31 - T[(x * 0x07C4ACDDU) >> 27];
  return v ? r : 32;
}

int
main ()
{
  if (__SIZEOF_INT__ * __CHAR_BIT__ != 32)
    return 0;
  if (f (0) != 32 || g (0) != 32)
    __builtin_abort ();
  for (int i = 0; i < 32; i++)
    if (f (1U << i) != 31 - i || g (1U << i) != 31 - i)
      __builtin_abort ();
  return 0;
}
