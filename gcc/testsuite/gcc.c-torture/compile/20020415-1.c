/* Check that floating point casts of integer operations don't ICE.  */
/* The first of these routines caused problems for a patch, that wasn't
   otherwise caught by a full bootstrap, the regression test suite or
   SPEC CPU2000.  */

double
andop (unsigned int x)
{
  return x & 1;
}

double
orop (unsigned int x)
{
  return x | 1;
}

double
notop (unsigned int x)
{
  return ~x;
}
