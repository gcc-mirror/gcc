[[gnu::nonnull]]
static
void init_x(int cond, int **x, int *y)
{
  if (!cond)
    return;
  *x = y;
}

int foo(int cond)
{
  int *x;
  int y = 7;

  if (cond < 2)
    return -1;
  init_x(cond, &x, &y);

  return *x;
}
