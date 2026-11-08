/* { dg-do compile } */
/* { dg-options "-O2 -mno-lzcnt -mno-bmi -fdump-tree-forwprop1 -fdump-tree-optimized" } */

/* Without LZCNT and BMI the value of CLZ and CTZ at zero is not defined
   on x86, so the table lookups must not become calls that carry a value
   at zero, and a caller's guard against zero must survive.  A runtime
   test can't check the CTZ case reliably: at -O1 and above it expands to
   rep bsf, which BMI1 hardware executes as tzcnt, returning 32 for zero
   even without the guard.  */

static const unsigned TC[32] = {
  0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30,
  8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31
};

static const unsigned char TT[32] = {
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

unsigned
clz_fb (unsigned v)
{
  v |= v >> 1;
  v |= v >> 2;
  v |= v >> 4;
  v |= v >> 8;
  v |= v >> 16;
  return 31 - TC[(v * 0x07C4ACDDU) >> 27];
}

unsigned
ctz_fb (unsigned v)
{
  return TT[((v & -v) * 0x077CB531U) >> 27];
}

unsigned
clz_guard (unsigned v)
{
  return v ? clz_fb (v) : 32;
}

unsigned
ctz_guard (unsigned v)
{
  return v ? ctz_fb (v) : 32;
}

unsigned
clz_sel (unsigned v)
{
  unsigned x = v;
  x |= x >> 1;
  x |= x >> 2;
  x |= x >> 4;
  x |= x >> 8;
  x |= x >> 16;
  unsigned r = 31 - TC[(x * 0x07C4ACDDU) >> 27];
  return v ? r : 32;
}

unsigned
ctz_sel (unsigned v)
{
  unsigned r = TT[((v & -v) * 0x077CB531U) >> 27];
  return v ? r : 32;
}

/* { dg-final { scan-tree-dump-times "\\.CLZ \\(" 3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\.CTZ \\(" 3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "\\.C\[LT\]Z \\(\[^\\n,\]*, " "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "\\.C\[LT\]Z \\(\[^\\n,\]*, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "PHI <32\\(" 4 "optimized" } } */
