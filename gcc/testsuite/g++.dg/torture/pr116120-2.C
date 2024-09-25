// { dg-do run }
// PR tree-optimization/116120

// The optimization for `(a ? x : y) == (b ? x : y)`
// missed that x and y could be the same value
// This can't be done if x and y support NaNs since `NaN == NaN` is always false
// And dominates the whole expression rather than supporting it.

typedef int v1si __attribute((__vector_size__(1 * sizeof(int))));
typedef float v1sf __attribute((__vector_size__(1 * sizeof(float))));

v1si f1(v1si a, v1si b, v1si c, v1si d, v1sf e, v1sf f) __attribute__((noinline));
v1si f1(v1si a, v1si b, v1si c, v1si d, v1sf e, v1sf f) {
  v1sf X = a == b ? e : f;
  v1sf Y = c == d ? f : e;
  return (X == Y); // ~(X == Y ? -1 : 0) (x ^ Y)
}

int f2(int a, int b, int c, int d, float e, float f) __attribute__((noinline));
int f2(int a, int b, int c, int d, float e, float f) {
  float X = a == b ? e : f;
  float Y = c == d ? f : e;
  return (X == Y) ? -1 : 0; // ~(X == Y ? -1 : 0) (x ^ Y)
}

int main()
{
  v1si a = {0};
  v1si b = {0}; // a == b, true
  v1si c = {2};
  v1si d = {3}; // c == b, false
  v1sf e;
  v1sf f;

  /* Test signed 0s. */
  e = (v1sf){0.0};
  f = -e;
  v1si r = f1(a,b,c,d,e, f);
  int r1 = f2(a[0], b[0], c[0], d[0], e[0], f[0]);
  if (r[0] != r1)
    __builtin_abort();

  /* Test NaNs */
#if __FLT_HAS_QUIET_NAN__
  e = (v1sf){__builtin_nanf("")};
  f = e;
  /* Test NaNs */
  r = f1(a,b,c,d,e, f);
  r1 = f2(a[0], b[0], c[0], d[0], e[0], f[0]);
  if (r[0] != r1)
    __builtin_abort();
#endif
}
