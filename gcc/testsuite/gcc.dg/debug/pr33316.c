/* PR debug/33316 */

int
foo (void *x, int y)
{
  const struct { int d[y]; } *a = x;
  return a[0].d[0];
}

int
bar (void *x, int y)
{
  const struct S { int d[y]; } *a = x;
  return a[0].d[0];
}
