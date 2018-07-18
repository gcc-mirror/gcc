/* PR rtl-optimization/85376 */
/* { dg-do run { target int128 } } */
/* { dg-options "-Og -fno-dce -fgcse -fno-tree-ccp -fno-tree-copy-prop -Wno-psabi" } */

typedef unsigned int U __attribute__ ((vector_size (64)));
typedef unsigned __int128 V __attribute__ ((vector_size (64)));
unsigned int e, i, l;
unsigned char f;
U g, h, k, j;

static inline V
foo (unsigned char n, unsigned short o, unsigned int p, U q, U r, U s)
{
  unsigned int t;
  o <<= 5;
  q[7] >>= __builtin_add_overflow (0xfffffff0, __builtin_ffs (n), &s[5]);
  t = __builtin_ffs (g[7]);
  e *= __builtin_sub_overflow (o, t, &f);
  return f + (V) g + (V) h + (V) q + i + (V) j + (V) s + (V) k + l;
}

int
main ()
{
  if (__SIZEOF_INT128__ != 16 || __SIZEOF_INT__ != 4 || __CHAR_BIT__ != 8)
    return 0;
  V x = foo (0, 1, 5, (U) { }, (U) { }, (U) { });
  for (unsigned i = 0; i < 4; i++)
    if ((unsigned int) x[i] != 0x20)
      __builtin_abort ();
  return 0;
}
