/* PR tree-optimization/47140 */

static inline int
foo (int x, short y)
{
  return y == 0 ? x : x + y;
}

static inline unsigned short
bar (unsigned short x, unsigned char y)
{
  return x - y;
}

int w;

int baz (void);

int
test (void)
{
  int i;
  for (i = 0; i < 50; i++)
    w += foo ((unsigned char) (1 + baz ()) >= bar (0, 1), 0);
}
