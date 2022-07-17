#/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -mno-ssse3" } */

extern short arr_108[][4][2][24][12], arr_110[][4][2][24][12];
void test() {
  for (unsigned a = 0; a < 2; a += 2)
    for (unsigned b = 4; b < 22; b++)
      for (int c = 1; c < 11; c++)
        arr_110[0][0][a][b][c] = (unsigned char)arr_108[0][0][a][b][c];
}

