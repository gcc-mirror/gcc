/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512" } */

extern long var_22;
extern int arr_3[];
extern int arr_4[][20][9];
short a;
void test(unsigned short b, unsigned char e, long long g) {
  for (long c = 0; c < 20ULL; c = g)
    for (short d = 0; d < 9; d++)
      for (char f = e; f < 8; f += 4) {
        arr_3[f] = 0;
        var_22 = ~(unsigned)b;
        arr_4[c][d][f] = a;
      }
}
