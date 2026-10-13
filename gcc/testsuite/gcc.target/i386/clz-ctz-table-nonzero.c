/* { dg-do compile } */
/* { dg-options "-O2 -mbmi -mlzcnt -fdump-tree-forwprop1 -fdump-tree-optimized" } */

/* With BMI and LZCNT the value of CTZ and CLZ at zero is defined, but here
   the table lookups are only reached for a non-zero input.  The calls must
   not carry a value at zero then: it would be 0 rather than the target's
   value, and once the guard is folded away the call would claim the wrong
   result for zero.  */

static const unsigned TC[32] = {
  0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
  8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31
};

static const unsigned char TT[32] = {
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

unsigned
clz_nz (unsigned v)
{
  if (v)
    {
      unsigned x = v;
      x |= x >> 1;
      x |= x >> 2;
      x |= x >> 4;
      x |= x >> 8;
      x |= x >> 16;
      return 31 - TC[(x * 0x07C4ACDDU) >> 27];
    }
  return 0;
}

unsigned
ctz_nz (unsigned v)
{
  if (v)
    return TT[((v & -v) * 0x077CB531U) >> 27];
  return 0;
}

/* { dg-final { scan-tree-dump-times "\\.CLZ \\(" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\.CTZ \\(" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "\\.C\[LT\]Z \\(\[^\\n,\]*, " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "\\.C\[LT\]Z \\(\[^\\n,\]*, " "optimized" } } */
