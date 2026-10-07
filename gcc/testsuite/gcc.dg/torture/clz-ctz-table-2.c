/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

/* The table entries for a zero input do not survive the computation of
   the recognized counts in int.  They are selected in the type of the
   table.  */

static const long long tab[64] = {
  0x100000005LL, 0, 0, 0, 3, 0, 0, 20, 0, 4, 0, 11, 7, 0, 0, 21,
  30, 1, 0, 5, 28, 0, 12, 0, 17, 8, 0, 14, 0, 0, 0, 22,
  31, 0, 2, 19, 0, 10, 6, 0, 29, 0, 27, 0, 16, 13, 0, 0,
  0, 18, 9, 0, 0, 26, 15, 0, 0, 0, 25, 0, 0, 24, 23, 0 };

__attribute__((noipa)) long long
f (unsigned v)
{
  return tab[((unsigned) ((v & -v) * 0x2265b1f5U)) >> 26];
}

/* The CLZ table entry fits int only after it is subtracted from 31.  */

static const long long tab2[64] = {
  0x80000000LL, 29, 0, 6, 0, 12, 0, 0, 2, 0, 5, 0, 1, 0, 0, 0,
  0, 31, 0, 0, 25, 0, 16, 26, 0, 22, 0, 17, 8, 27, 0, 0,
  0, 23, 14, 20, 0, 18, 0, 9, 0, 28, 0, 11, 0, 4, 0, 0,
  0, 30, 24, 15, 0, 21, 7, 0, 0, 13, 19, 0, 0, 0, 10, 3 };

__attribute__((noipa)) long long
g (unsigned x)
{
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  return tab2[(x * 0xbb968a43U) >> 26];
}

/* The unsigned entry does not fit int.  */

static const unsigned tab3[64] = {
  0xffffffffU, 0, 0, 0, 3, 0, 0, 20, 0, 4, 0, 11, 7, 0, 0, 21,
  30, 1, 0, 5, 28, 0, 12, 0, 17, 8, 0, 14, 0, 0, 0, 22,
  31, 0, 2, 19, 0, 10, 6, 0, 29, 0, 27, 0, 16, 13, 0, 0,
  0, 18, 9, 0, 0, 26, 15, 0, 0, 0, 25, 0, 0, 24, 23, 0 };

__attribute__((noipa)) unsigned
h (unsigned v)
{
  return tab3[((unsigned) ((v & -v) * 0x2265b1f5U)) >> 26];
}

int
main (void)
{
  if (f (0) != 0x100000005LL || f (1) != 0 || f (0x80000000U) != 31)
    __builtin_abort ();
  if (g (0) != 0x80000000LL || g (1) != 0 || g (0x80000000U) != 31
      || g (0x1234) != 12)
    __builtin_abort ();
  if (h (0) != 0xffffffffU || h (1) != 0 || h (0x80000000U) != 31)
    __builtin_abort ();
  return 0;
}
