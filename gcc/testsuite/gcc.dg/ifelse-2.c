/*
{ dg-do run }
{ dg-options "-O2" }
*/

extern void abort (void);

enum Status
{
  P_ON_LOWER = -4,
  P_ON_UPPER = -2,
  P_FREE = -1
};

void
foo (enum Status *stat, double newUpper, double lower, double max)
{
  if (newUpper >= max)
    *stat = P_FREE;
  else if (newUpper == lower)
    *stat = P_ON_LOWER;
}

int
main ()
{
  enum Status stat = P_ON_UPPER;

  foo (&stat, 5.0, -10.0, 10.0);

  if (stat != P_ON_UPPER)
    abort ();
  return 0;
}
