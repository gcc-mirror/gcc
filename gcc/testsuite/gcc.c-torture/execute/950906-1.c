g (int i)
{
}

f (int i)
{
  g (0);
  while ( ({ i--; }) )
    g (0);
}

main ()
{
  f (10);
  exit (0);
}
