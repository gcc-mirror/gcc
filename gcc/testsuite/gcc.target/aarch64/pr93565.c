/* { dg-do compile } */
/* { dg-options "-O2" } */

static const unsigned long long magic = 0x03f08c5392f756cdULL;

static const char table[64] = {
     0,  1, 12,  2, 13, 22, 17,  3,
    14, 33, 23, 36, 18, 58, 28,  4,
    62, 15, 34, 26, 24, 48, 50, 37,
    19, 55, 59, 52, 29, 44, 39,  5,
    63, 11, 21, 16, 32, 35, 57, 27,
    61, 25, 47, 49, 54, 51, 43, 38,
    10, 20, 31, 56, 60, 46, 53, 42,
     9, 30, 45, 41,  8, 40,  7,  6,
};

static inline int ctz1 (unsigned long long  b)
{
  unsigned long long lsb = b & -b;
  return table[(lsb * magic) >> 58];
}

void f (unsigned long long x, int *p)
{
  if (x != 0)
    {
      int a = ctz1 (x);
      *p = a | p[a];
    }
}

/* { dg-final { scan-assembler-times "rbit\t" 1 } } */
/* { dg-final { scan-assembler-times "clz\t" 1 } } */

