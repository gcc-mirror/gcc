/* { dg-options "-O2 -fgraphite-identity -fdump-tree-graphite-all" } */


int winner, numf2s;
double **tds;
double d, tsum;

typedef struct {
  double y;
} xyz;

xyz *Y;
int ti;

double
match (void)
{
  int tj, tresult;

  for (tj = 0; tj < numf2s; tj++)
    if (tj == winner
	&& Y[tj].y > 0)
      tsum += tds[ti][tj] * d;

  return tsum;
}

/* There should be no loops generated for this testcase, instead we
   should generate the following:

   | if (winner >= 0 && winner < numf2s && Y[winner].y > 0)
   |   tsum += tds[ti][winner] * d;

   For the moment this is XFAILed as this loop is not detected as a
   SCoP by graphite: we depend on data in one of the conditions,
   "Y[winner].y > 0".  This could be fixed when we will use predicates
   for such cases.  */

/* { dg-final { scan-tree-dump-times "loop_1" 0 "graphite" } } */
