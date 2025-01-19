/* { dg-do compile } */
/* { dg-options "-O3 -mvis3" } */

long c;
int d[10];
int e;
void g() {
  int b = 1 & e;
  int *f = d;
  b = -b;
  c = 0;
  for (; c < 10; c++) {
    int h = f[c] ^ c;
    h &= b;
    f[c] ^= h;
  }
}
