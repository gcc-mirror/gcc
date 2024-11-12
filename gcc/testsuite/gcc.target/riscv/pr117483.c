/* { dg-do compile } */
/* { dg-options "-fsigned-char -fno-strict-aliasing -fwrapv -march=rv64gcv_zvl256b -mabi=lp64d -O3" } */

char a, b, f;
short c, h;
int d[5];
int e;
long *g;
short (*i)[4];
int main() {
  e = 906784;
  char *j = &f;
  short *k = &h;
  for (short l = 0; l < 23; l += 740314495218734 - 29738)
    for (unsigned char m = ~!g[l] - 255; m < 24; m += 3) {
      a = k[m];
      b += j[1] ?: d[6];
      c ^= i[010][l];
    }
}
