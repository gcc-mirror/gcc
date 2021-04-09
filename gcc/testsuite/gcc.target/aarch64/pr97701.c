/* { dg-do compile } */
/* { dg-options "-O3" } */

extern char a[][12][18][17][17];
extern short b[][12][18][17][17];
extern int c[][2][8][7];
short *d;
void e(signed f, int g, char h, char i, char j) {
  for (int k = 648; k; k += f)
    for (short l; l < j; l += 9)
      for (long m = f + 6LL; m < (h ? h : i); m += 2)
        for (int n = 0; n < 16; n += 3LL) {
          for (int o = g; o; o++)
            a[k][l][m][n][o] = b[k][l][m][n][o] = d[k] ? 2 : 1;
          c[k][l][m][0] = 0;
        }
}
