/* { dg-do compile } */
/* { dg-options "-O -mbmi" } */

int ctz1 (unsigned x)
{
  static const char table[32] =
    {
      0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
      31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
    };

  return table[((unsigned)((x & -x) * 0x077CB531U)) >> 27];
}

int ctz2 (unsigned x)
{
#define u 0
  static short table[64] =
    {
      32, 0, 1,12, 2, 6, u,13, 3, u, 7, u, u, u, u,14,
      10, 4, u, u, 8, u, u,25, u, u, u, u, u,21,27,15,
      31,11, 5, u, u, u, u, u, 9, u, u,24, u, u,20,26,
      30, u, u, u, u,23, u,19,29, u,22,18,28,17,16, u
    };

  x = (x & -x) * 0x0450FBAF;
  return table[x >> 26];
}

int ctz3 (unsigned x)
{
  static int table[32] =
    {
      0, 1, 2,24, 3,19, 6,25, 22, 4,20,10,16, 7,12,26,
      31,23,18, 5,21, 9,15,11,30,17, 8,14,29,13,28,27
    };

  if (x == 0) return 32;
  x = (x & -x) * 0x04D7651F;
  return table[x >> 27];
}

/* { dg-final { scan-assembler-times "tzcntl\t" 3 } } */
/* { dg-final { scan-assembler-times "andl\t" 1 } } */
/* { dg-final { scan-assembler-not "neg" } } */
/* { dg-final { scan-assembler-not "imul" } } */
/* { dg-final { scan-assembler-not "shr" } } */
