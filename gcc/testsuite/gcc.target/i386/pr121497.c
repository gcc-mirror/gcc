/* { dg-do compile } */
/* { dg-options "-O3 -msse2 -w" } */

extern void a(int *);
int q;
void b(int c, int d, int e, int f, int g, int h) {
  int t[] = {c, d, e, f, g, h};
  a(t);
}
int main() {
  int k[2], i = 0, *p();
  if (q) {
    for (; (int)p + i < 2; i++)
      k[i] = -1294967296;
    b(k[0] + 7, k[0] + 9, k[0] + 6, k[0] + 9, k[0] + 9, k[0] + 6);
  }
  return 0;
}
