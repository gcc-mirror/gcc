
int bar1 (int);
int bar2 (int);

void foo (int cmd)
{
  __label__ start;
  int x = 0;

  void nonlocal_goto (void)
  {
    x++;
    goto start;
  }

start:
  while (bar1 (x))
    if (bar2 (x))
      nonlocal_goto ();
}
