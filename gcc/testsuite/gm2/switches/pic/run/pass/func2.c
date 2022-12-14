extern void myexternfunc (void);

static void mystaticfunc (void)
{
}

void mypublicfunc (void)
{
  void mynestedfunc (void)
  {
  }
}

static void (*p)(void);

static void foo (void)
{
  p = mystaticfunc;
  p();
  p = mypublicfunc;
  p();
  p = myexternfunc;
  p();
}
