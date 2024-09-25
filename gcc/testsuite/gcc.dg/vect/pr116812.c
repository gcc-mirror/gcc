/* { dg-do compile } */
/* { dg-additional-options "-O2 -fno-tree-dce -fno-tree-dse" } */

int a, b, c, d, e, f[2], g, h;
int k(int j) { return 2 >> a ? 2 >> a : a; }
int main() {
  int i;
  for (; g; g = k(d = 0))
    ;
  if (a)
    b && h;
  for (e = 0; e < 2; e++)
    c = d & 1 ? d : 0;
  for (i = 0; i < 2; i++)
    f[i] = 0;
  return 0;
}
