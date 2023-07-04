/* { dg-do run } */

int f, g, a, c;
unsigned char k = 204;
unsigned char *l = &k;
short m, n;
static long b;
unsigned *h = &c;
unsigned **i = &h;
int p(unsigned char *aa) {
  aa[0] && aa[1] && aa[2];
  return 1;
}
int q(unsigned char c) {
  unsigned char d[] = {c};
  int e = p(d);
  return e;
}
int r(int j, int h) {
  f = h / 4;
  g = f * 6;
  return g;
}
short s() { return **i; }
void t() {
  for (; r(9, *l) <= 1;) {
    int j;
    long *o = &b;
    *o = 0 >= 0;
    for (; q(0) + a > 1; a++)
      *o = 0 > m;
    j = s();
    for (; a;)
      n = j;
    for (; (unsigned char)(1 + k + b) + k; --k)
      ;
  }
}
int main() { t(); }
