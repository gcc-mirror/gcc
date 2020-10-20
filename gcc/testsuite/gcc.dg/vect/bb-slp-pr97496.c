/* { dg-do compile } */

int a;
int b[1024];
void c(unsigned g) {
  if (a) {
    long e = g, d = e;
    int f = 0;
    for (; f < 4; f++) {
      b[f] = d;
      d >>= 8;
    }
  }
}
