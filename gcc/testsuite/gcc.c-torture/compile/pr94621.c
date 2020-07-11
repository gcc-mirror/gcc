/* PR tree-optimization/94621 */

struct S { int c, e[]; };

static inline int
foo (struct S *m, int r, int c)
{
  int (*a)[][m->c] = (int (*)[][m->c])&m->e;
  return (*a)[r][c];
}

void
bar (struct S *a)
{
  foo (a, 0, 0);
}
