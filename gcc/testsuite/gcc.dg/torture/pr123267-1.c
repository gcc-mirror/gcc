/* { dg-do compile } */

/* PR rtl-optimization/123267 */

int j(long e, int k, int i) {
  return (i&-3) == 0 ? k : k - e;
}
