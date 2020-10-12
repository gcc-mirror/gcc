/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O -mbmi" } */

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

int ctz4 (unsigned long long x)
{
  unsigned long long lsb = x & -x;
  return table[(lsb * magic) >> 58];
}

/* { dg-final { scan-assembler-times "tzcntq\t" 1 } } */
/* { dg-final { scan-assembler-times "andl\t" 1 } } */
/* { dg-final { scan-assembler-not "negq" } } */
/* { dg-final { scan-assembler-not "imulq" } } */
/* { dg-final { scan-assembler-not "shrq" } } */
