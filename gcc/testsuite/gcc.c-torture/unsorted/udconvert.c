double
unsigned_to_double1 (u)
     unsigned u;
{
  double d;
  d = (int) u;			/* convert as from a *signed* integer */
  return ((int) u < 0)
    ? d + 4294967296.0
      : d;
}

/* Alternatively */

double
unsigned_to_double2 (u)
     unsigned u;
{
  double d;
  u -= 2147483648;		/* complement sign bit */
  d = (int) u;			/* convert as from a *signed* integer */
  return d + 2147483648.0;
}

unsigned
double_to_unsigned (d)
     double d;
{
  d += 2147483648.0;
  return ((int) d) - 2147483648;
}
