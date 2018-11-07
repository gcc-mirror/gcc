/* { dg-do compile } */
/* { dg-options "-w -Ofast -fnon-call-exceptions -ftrapping-math -fdump-tree-recip" } */

/* Check that the recip_sqrt optimization does not trigger here, causing an
   ICE due to EH info.  */


double res, res2, tmp;
void
foo1 (double a, double b)
{
  try {
    tmp = 1.0 / __builtin_sqrt (a);
    res = tmp * tmp;
    res2 = a * tmp;
  }
  catch (...)
    { ; }
}

void
foo4 (double a, double b, int c, int d)
{
  try {
    tmp = 1.0 / __builtin_sqrt (a);
  }
  catch (...)
    {
      if (c)
	res = tmp * tmp;

      if (d)
	res2 = a * tmp;
    }
}

void
foo5 (double a, double b, int c, int d)
{
  try {
    tmp = 1.0 / __builtin_sqrt (a);
    res = tmp * tmp;

    if (d)
      res2 = a * tmp;
  }
  catch (...)
    { ; }
}
