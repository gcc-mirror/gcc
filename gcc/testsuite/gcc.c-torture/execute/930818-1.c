static double one = 1.0;

f()
{
  int colinear;
  colinear = (one == 0.0);
  if (colinear)
    abort ();
  return colinear;
}
main()
{
  if (f()) abort();
  exit (0);
}
