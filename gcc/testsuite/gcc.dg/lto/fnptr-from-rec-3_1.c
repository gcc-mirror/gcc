struct fnptrs
{
  void (*first)(void);
  int (*dostuff)(int);
};

extern struct fnptrs fp;

int noop (int n)
{
  return n;
}


void __attribute__ ((noinline))
init (void)
{
  fp.dostuff = noop;
}
