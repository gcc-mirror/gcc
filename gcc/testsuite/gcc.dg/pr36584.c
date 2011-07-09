/* { dg-do run } */
/* { dg-options "-O2 -lm" } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-require-effective-target sse2_runtime { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

extern double fabs (double);
extern void abort (void);

const int MAX_ITERATIONS = 50;
const double SMALL_ENOUGH = 1.0e-10;
const double RELERROR = 1.0e-12;

typedef struct p
{
  int ord;
  double coef[7];
}
polynomial;

static double
polyeval (double x, int n, double *Coeffs)
{
  register int i;
  double val;

  val = Coeffs[n];
  for (i = n - 1; i >= 0; i--)
    val = val * x + Coeffs[i];

  return (val);
}

static int
regula_falsa (int order, double *coef, double a, double b, double *val)
{
  int its;
  double fa, fb, x, fx, lfx;

  fa = polyeval (a, order, coef);
  fb = polyeval (b, order, coef);

  if (fa * fb > 0.0)
    return 0;

  if (fabs (fa) < SMALL_ENOUGH)
    {
      *val = a;
      return 1;
    }

  if (fabs (fb) < SMALL_ENOUGH)
    {
      *val = b;
      return 1;
    }

  lfx = fa;

  for (its = 0; its < MAX_ITERATIONS; its++)
    {
      x = (fb * a - fa * b) / (fb - fa);
      fx = polyeval (x, order, coef);
      if (fabs (x) > RELERROR)
	{
	  if (fabs (fx / x) < RELERROR)
	    {
	      *val = x;
	      return 1;
	    }
	}
      else
	{
	  if (fabs (fx) < RELERROR)
	    {
	      *val = x;
	      return 1;
	    }
	}

      if (fa < 0)
	{
	  if (fx < 0)
	    {
	      a = x;
	      fa = fx;
	      if ((lfx * fx) > 0)
		fb /= 2;
	    }
	  else
	    {
	      b = x;
	      fb = fx;
	      if ((lfx * fx) > 0)
		fa /= 2;
	    }
	}
      else
	{
	  if (fx < 0)
	    {
	      b = x;
	      fb = fx;
	      if ((lfx * fx) > 0)
		fa /= 2;
	    }
	  else
	    {
	      a = x;
	      fa = fx;
	      if ((lfx * fx) > 0)
		fb /= 2;
	    }
	}

      if (fabs (b - a) < RELERROR)
	{
	  *val = x;
	  return 1;
	}

      lfx = fx;
    }

  return 0;
}

static int
numchanges (int np, polynomial * sseq, double a)
{
  int changes;
  double f, lf;
  polynomial *s;
  changes = 0;

  lf = polyeval (a, sseq[0].ord, sseq[0].coef);

  for (s = sseq + 1; s <= sseq + np; s++)
    {
      f = polyeval (a, s->ord, s->coef);
      if (lf == 0.0 || lf * f < 0)
	changes++;

      lf = f;
    }

  return changes;
}

int
sbisect (int np, polynomial * sseq, double min_value, double max_value,
	 int atmin, int atmax, double *roots)
{
  double mid;
  int n1, n2, its, atmid;

  if ((atmin - atmax) == 1)
    {
      if (regula_falsa (sseq->ord, sseq->coef, min_value, max_value, roots))
	return 1;
      else
	{
	  for (its = 0; its < MAX_ITERATIONS; its++)
	    {
	      mid = (min_value + max_value) / 2;
	      atmid = numchanges (np, sseq, mid);
	      if ((atmid < atmax) || (atmid > atmin))
		return 0;

	      if (fabs (mid) > RELERROR)
		{
		  if (fabs ((max_value - min_value) / mid) < RELERROR)
		    {
		      roots[0] = mid;
		      return 1;
		    }
		}
	      else
		{
		  if (fabs (max_value - min_value) < RELERROR)
		    {
		      roots[0] = mid;
		      return 1;
		    }
		}

	      if ((atmin - atmid) == 0)
		min_value = mid;
	      else
		max_value = mid;
	    }

	  roots[0] = mid;
	  return 1;
	}
    }

  for (its = 0; its < MAX_ITERATIONS; its++)
    {
      mid = (min_value + max_value) / 2;
      atmid = numchanges (np, sseq, mid);
      if ((atmid < atmax) || (atmid > atmin))
	return 0;

      if (fabs (mid) > RELERROR)
	{
	  if (fabs ((max_value - min_value) / mid) < RELERROR)
	    {
	      roots[0] = mid;
	      return 1;
	    }
	}
      else
	{
	  if (fabs (max_value - min_value) < RELERROR)
	    {
	      roots[0] = mid;
	      return 1;
	    }
	}

      n1 = atmin - atmid;
      n2 = atmid - atmax;

      if ((n1 != 0) && (n2 != 0))
	{
	  n1 = sbisect (np, sseq, min_value, mid, atmin, atmid, roots);
	  n2 = sbisect (np, sseq, mid, max_value, atmid, atmax, &roots[n1]);

	  return (n1 + n2);
	}

      if (n1 == 0)
	min_value = mid;
      else
	max_value = mid;
    }

  roots[0] = mid;
  return 1;
}

int
main ()
{
  polynomial sseq[7] = {
    {6, {0.15735259075109281, -5.1185263411378736, 1.8516070705868664,
	 7.348009172322695, -2.2152395279161343, -2.7543325329350692, 1.0}},
    {5, {-0.8530877235229789, 0.61720235686228875, 3.6740045861613475,
	 -1.4768263519440896, -2.2952771107792245, 1.0}},
    {4, {0.13072124257049417, 2.2220687798791126, -1.6299431586726509,
	 -1.6718404582408546, 1.0}},
    {3, {0.86776597575462633, -2.1051099695282511, -0.49008580100694688,
	 1.0}},
    {2, {-11.117984175064155, 10.89886635045883, 1.0}},
    {1, {0.94453099602191237, -1.0}},
    {0, {-0.068471716890574186}}
  };

  double roots[7];
  int nroots;

  nroots = sbisect (6, sseq, 0.0, 10000000.0, 5, 1, roots);
  if (nroots != 4)
    abort ();

  return 0;
}
