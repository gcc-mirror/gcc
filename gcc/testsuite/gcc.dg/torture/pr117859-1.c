/* { dg-do compile } */
/* { dg-options "-O2 -fno-ssa-phiopt -fno-expensive-optimizations" } */

/* PR tree-optimization/117859 */

unsigned char a;
int b, c, d, e, g, h, j, k;
struct {
  int f : 1;
} i;
char l(int m) { return a > 255 >> m ? 0 : m; }
int main() {
  if (i.f)
    d = b;
  k = e = 1;
  for (; e; e--)
    g = 0;
  h = g ? 1 << g : 1;
  c = h || i.f;
  while (b)
    j = l(k && i.f);
  return 0;
}
