/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-dse -fno-tree-dce" } */
/* PR rtl-optimization/123294 */

typedef unsigned a;
int b;
void g(__attribute__((__vector_size__(4 * sizeof(a)))) a *);
a d() {
  __attribute__((__vector_size__(4 * sizeof(a)))) a e[5];
  if (0 > b) {
    __attribute__((__vector_size__(8 * sizeof(a)))) a f = {b, b};
    e[0] = __builtin_shufflevector(f, f, 0, 1, 2, 3);
  }
  return 1;
}
