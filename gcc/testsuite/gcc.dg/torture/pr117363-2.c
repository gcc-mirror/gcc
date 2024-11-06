/* { dg-do compile } */

/* PR tree-optimization/117363 */
/* ldist produces `s != 0 ? s - 1 : 0`  (with casts) and that
   the match pattern which messed up the converts. */

void f(int *array, long t) {
  if (!t) return;
  unsigned long s = ~t;
  for (long i = 0; i < s; i++)
    array[i] = 0;
}
