/* An info-less pointer (re-materialized by PRE after the last points-to
   run) may still point to a non-escaped local; the dependence against
   that local must be preserved.  Execution test: the store stream
   through 'tab[idx & 1]' aliases 't' when idx is even.  */
/* { dg-do run } */
/* { dg-additional-options "-O3 -ffast-math" } */

extern void abort (void);

double res;
int lo, hi, idx;

void __attribute__ ((noipa))
f (int c)
{
  int n = hi - lo + 2;
  double t[n], u[n];
  double *tab[2] = { t, u };

  /* Load tab[idx&1] on both arms so PRE hoists it into a pretmp.  */
  if (c)
    res = tab[idx & 1][0];
  else
    res = tab[idx & 1][1];

  for (int k = 0; k < n; k++)
    t[k] = u[k] = k;

  for (int k = 3; k < n - 2; k++)
    {
      tab[idx & 1][k] = t[k - 1] + 1.0;   /* stores TO t when idx is even */
      res += t[k];                        /* must observe those stores    */
    }

  double s = 0;
  for (int k = 2; k < n - 2; k++)
    s += t[k] + u[k];
  res += s;
}

int
main (void)
{
  lo = 0; hi = 14; idx = 0;   /* idx even: store stream aliases t */
  res = 0;
  f (1);
  double expect = 0.0 + (3+4+5+6+7+8+9+10+11+12+13)
		  + 2.0 * (2+3+4+5+6+7+8+9+10+11+12+13);
  if (res != expect)
    abort ();
  return 0;
}
