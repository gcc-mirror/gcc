struct a
{
  int t;
  int t1;
};

int f(int i, int j)
{
  struct a *t;
  struct a t1 = {i, j};
  t = &t1;
  auto int g(void) __attribute__((noinline));
  int g(void)
  {
    return t->t + t->t1;
  }
  return g();
}
