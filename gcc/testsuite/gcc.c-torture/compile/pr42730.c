union bzz
{
  unsigned *pa;
  void *pv;
};

void foo (void)
{
  union bzz u;
  void **x;
  void *y = 0;
  x = &u.pv;
  *x = y;
}
