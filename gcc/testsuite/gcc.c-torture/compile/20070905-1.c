/* PR tree-optimization/32772 */

struct S
{
  unsigned long bits[1];
};

void f1 (int, unsigned long *);
int f2 (void);
int f3 (int, unsigned long *);
int f4 (int, unsigned long *);

static inline __attribute__ ((always_inline))
void baz (int x, volatile struct S *y)
{
  f1 (x, y->bits);
}

static int
bar (int x, struct S *y)
{
  int n;
  if (__builtin_constant_p (x) ? f3 (x, y->bits) : f4 (x, y->bits))
    baz (x, y);
  for (n = f2 (); n < 8; n = f2 ())
    f3 (n, y->bits);
}

void
foo (int x, int y)
{
  struct S m;
  while ((y = bar (x, &m)) >= 0);
}
