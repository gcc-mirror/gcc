// { dg-do run }
// PR tree-optimization/116120

// The optimization for `(a ? x : y) != (b ? x : y)`
// missed that x and y could be the same value.

typedef int v4si __attribute((__vector_size__(1 * sizeof(int))));
v4si f1(v4si a, v4si b, v4si c, v4si d, v4si e, v4si f) {
  v4si X = a == b ? e : f;
  v4si Y = c == d ? e : f;
  return (X != Y); // ~(X == Y ? -1 : 0) (x ^ Y)
}

int f2(int a, int b, int c, int d, int e, int f) {
  int X = a == b ? e : f;
  int Y = c == d ? e : f;
  return (X != Y) ? -1 : 0; // ~(X == Y ? -1 : 0) (x ^ Y)
}

int main()
{
  v4si a = {0};
  v4si b = {0}; // a == b, true
  v4si c = {2};
  v4si d = {3}; // c == b, false
  v4si e = {0};
  v4si f = e;
  v4si r = f1(a,b,c,d,e, f);
  int r1 = f2(a[0], b[0], c[0], d[0], e[0], f[0]);
  if (r[0] != r1)
    __builtin_abort();
}
