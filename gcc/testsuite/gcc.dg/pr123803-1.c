/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-overflow" } */
/* PR tree-optimization/123803 */

/* SLSR with wrapping pointers converted the new
   nhs2 into the pointer type instead of sizetype. */
int f(int *x1,__SIZE_TYPE__ n) {
  return *(x1 + n) +
  *(x1 + 3 * n);
}
