void foo(int x)
{
  if (x > 3)
    {;}
  else
    bar();
  x = 9;
}

main()
{
  int j;

  foo(j);
  return j;
}
