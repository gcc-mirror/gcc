
const double dnan = 1.0/0.0 - 1.0/0.0;
double x = 1.0;

extern void link_error ();

main ()
{
#if ! defined (__vax__) && ! defined (_CRAY)
  /* NaN is an IEEE unordered operand.  All these test should be false.  */
  if (dnan == dnan)
    link_error ();
  if (dnan != x)
    x = 1.0;
  else
    link_error ();

  if (dnan < x)
    link_error ();
  if (dnan > x)
    link_error ();
  if (dnan <= x)
    link_error ();
  if (dnan >= x)
    link_error ();
  if (dnan == x)
    link_error ();
#endif
  exit (0);
}

