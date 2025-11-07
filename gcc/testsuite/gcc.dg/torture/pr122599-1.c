/* { dg-do compile } */
/* PR tree-optimization/122599 */

void f(int *x, unsigned n) {
  for (int i = 0; i < 5; i++)
    while ((int)--n >= 0)
      x[0] = 0;
}
