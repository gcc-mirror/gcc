void bar (void);

void foo(int x)
{
  if (x > 3)
    {;}
  else
    bar();
  x = 9;
}

int
main(void)
{
  int j;

  foo(j);
  return j;
}
