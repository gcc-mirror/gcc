struct foo
{
  unsigned y:1;
  unsigned x:32;
};

int
f (x)
     struct foo x;
{
  int t = x.x;
  if (t < 0)
    return 1;
  return t+1;
}

main ()
{
  struct foo x;
  x.x = -1;
  if (f (x) == 0)
    abort ();
  exit (0);
}
