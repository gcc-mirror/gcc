int var = 0;

g ()
{
  var = 1;
}

f ()
{
  int f2 = 0;

  if (f2 == 0)
    ;

  g ();
}

main ()
{
  f ();
  if (var != 1)
    abort ();
  exit (0);
}
