/* { dg-do compile } */
/* { dg-options "-march=armv9-a -O2" } */

/* PR rtl-optimization/123094 */

typedef long a;
typedef a __attribute__((vector_size(2 * sizeof(a)))) b;
typedef a __attribute__((vector_size(4 * sizeof(a)))) c;
typedef a __attribute__((vector_size(8 * sizeof(a)))) d;
a e;
int f, h;
d g;
[[gnu::noinline]] void j(b) {
  h = 1;
}
c k() {
  if (e)
    if (f)
      return (c){3};
  j((b){1});
  d l = {1, 1, 1, 1, 1, 1, 1};
  return __builtin_shufflevector(l, g, 2, 11, 4, 6);
}
