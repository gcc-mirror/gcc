/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop2-details -Wno-shift-count-negative" } */

int ctz_ice1 (int x)
{
  static const char table[32] =
    {
      0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
      31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
    };

  return table[((int)((x & -x) * -0x077CB531)) >> 27];
}

int ctz_ice2 (unsigned x)
{
  static const char table[32] =
    {
      0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
      31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
    };

  return table[((unsigned)((x & -x) * 0x077CB531U)) >> -27];
}

// This should never match
int ctz_fail (unsigned x)
{
  static const unsigned short int table[32] =
    u"\x0100\x021c\x0e1d\x0318\x161e\x0f14\x1119\x0804\x1b1f\x170d\x1315\x0710\x0c1a\x0612\x050b\x090a";

  return table[((x & -x) * 0x077CB531) >> 27];
}

/* { dg-final { scan-tree-dump-not {= \.CTZ} "forwprop2" } } */
