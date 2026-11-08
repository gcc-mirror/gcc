/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

/* Table-based count-trailing-zeros guarded against zero.  Same problem as
   clz-table-zero-1.c for CTZ.  */

static const unsigned char T[32] = {
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

static inline unsigned
ctz_fb (unsigned v)
{
  return T[((v & -v) * 0x077CB531U) >> 27];
}

__attribute__((noipa)) unsigned
f (unsigned v)
{
  return v ? ctz_fb (v) : 32;
}

__attribute__((noipa)) unsigned
g (unsigned v)
{
  unsigned r = T[((v & -v) * 0x077CB531U) >> 27];
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
    if (f (1U << i) != i || g (1U << i) != i)
      __builtin_abort ();
  return 0;
}
