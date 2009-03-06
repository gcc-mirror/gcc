/* PR middle-end/39360 */

static int a[] = { 1 };

static inline void
bar (int **x)
{
  static int *c[2] = { 0, a };
  *x = c[1];
}

int
foo (int **x)
{
  bar (x);
}
