f ()
{
  struct {
    int x : 18;
    int y : 14;
  } foo;

  foo.x = 10;
  foo.y = 20;

  return foo.y;
}

main ()
{
  if (f () != 20)
    abort ();
  exit (0);
}
