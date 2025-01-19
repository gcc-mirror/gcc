/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-fwrapv" } */

extern signed char a[];
long b;
signed char *c;
short *d;
int main() {
  for (int e = -63; e < 9; e += -4294967292) {
    int f_init = (b ? (8959267630 + c[e]) % 1365941252 : 0);
    for (int f = f_init; f < 9; f += 10) {
      int h = d[e + 1];
      a[e] = d[e - 2] ?: h;
    }
  }
}
