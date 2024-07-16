/* { dg-do compile } */
/* { dg-options "-O3 -mavx512vl --param vect-partial-vector-usage=2 -mtune=znver5 -mprefer-vector-width=512" } */
/* { dg-final { scan-assembler-not "kxor\[bw]" } } */

typedef unsigned long long BITBOARD;
BITBOARD KingPressureMask1[64], KingSafetyMask1[64];

void __attribute__((noinline))
foo()
{
  int i;

  for (i = 0; i < 64; i++) {
    if ((i & 7) == 0) {
      KingPressureMask1[i] = KingSafetyMask1[i + 1];
    } else if ((i & 7) == 7) {
      KingPressureMask1[i] = KingSafetyMask1[i - 1];
    } else {
      KingPressureMask1[i] = KingSafetyMask1[i];
    }
  }
}

BITBOARD verify[64] = {1, 1, 2, 3, 4, 5, 6, 6, 9, 9, 10, 11, 12, 13, 14, 14, 17, 17, 18, 19,
  20, 21, 22, 22, 25, 25, 26, 27, 28, 29, 30, 30, 33, 33, 34, 35, 36, 37, 38,
  38, 41, 41, 42, 43, 44, 45, 46, 46, 49, 49, 50, 51, 52, 53, 54, 54, 57, 57,
  58, 59, 60, 61, 62, 62};

int main()
{
  for (int i = 0; i < 64; ++i)
    KingSafetyMask1[i] = i;
  foo ();
  for (int i = 0; i < 64; ++i)
    if (KingPressureMask1[i] != verify[i])
      __builtin_abort ();
  return 0;
}
