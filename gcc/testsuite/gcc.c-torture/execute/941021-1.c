double glob_dbl;

f (pdbl, value)
     double *pdbl;
     double value;
{
  if (pdbl == 0)
    pdbl = &glob_dbl;

  *pdbl = value;
}

main ()
{
  f ((void *) 0, 55.1);

  if (glob_dbl != 55.1)
    abort ();
  exit (0);
}
