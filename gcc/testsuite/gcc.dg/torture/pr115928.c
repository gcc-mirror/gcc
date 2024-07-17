/* { dg-additional-options "-fgcse-sm" } */

int a[1], b, c;
struct {
  int d;
  int e;
  int : 8;
} f[1];
static int g;
char h, i, j;
void k(int l) { b = 5 ^ a[b ^ (l & 5)]; }
void m(long l) { k(c >> 6); }
int main() {
  g++;
  if (b) {
    h = 5 && j;
    if (h)
      h -= i;
    m(f[g].d);
    m(f[g].e);
  }
  return 0;
}
