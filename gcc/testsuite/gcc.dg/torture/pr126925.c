/* { dg-do compile } */

int a;
long b;
int c(long d) {
  for (;;)
    if (d)
      return b;
}
void e(int d) {
  unsigned long f = 6;
  int g;
  while (c(f)) {
    g = 0;
    for (; g < 2; g++) {
      a = 0;
      short h = d;
      f = d + f - (h + f + (a + 4 + f));
    }
  }
}
