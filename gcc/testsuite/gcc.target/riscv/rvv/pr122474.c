/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -O3 -fwrapv -w" } */

_Bool a;
void b(unsigned c[][8][8][8][8][8]) {
  for (int d = 1; d; d += 3)
    for (int e = 0; e < 11; e += 2)
      for (short f = 0; f < 1; f = 30482)
        for (short g = 0; g < 014; g++)
          a = ({
            int h = a;
            int i = c[2][f][d][d][d][d] ^ c[g][g][1][2][g][g];
            i ? h : i;
          });
}
