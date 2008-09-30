/* PR tree-optimization/37662 */

extern int baz (void);

static int
foo (void)
{
  return 1;
}

int
bar (void)
{
  return foo () >= 1 ^ (baz () || 0) || 0;
}
