/* Base pointers re-materialized by PRE after the last points-to run carry
   no SSA_NAME_PTR_INFO; dr_may_alias_p must fall back to the points-to
   solution recorded on the data reference (DR_PTR_INFO) instead of
   emitting runtime alias checks between provably disjoint objects.  */
/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-O3 -ffast-math" } */

struct desc { double *data; long span; };
struct desc g;
int lo, hi;
double res;

void
f (int c)
{
  int n = hi - lo + 2;
  double t[n], u[n];		/* VLAs -> __builtin_alloca_with_align */

  /* g.data is loaded on both arms, so PRE materialises it as a fresh
     "pretmp" pointer SSA name -- after the last points-to run.  */
  if (c)
    res = g.data[0];
  else
    res = g.data[1];

  for (int k = 2; k < n - 2; k++)
    {
      u[k] = g.data[k + 1] + g.data[k];
      t[k] = u[k] * (37.0 * (g.data[k + 1] + g.data[k])
		     - 8.0 * (g.data[k + 2] + g.data[k - 1]));
    }

  double s = 0;
  for (int k = 2; k < n - 2; k++)
    s += t[k] + u[k];
  res += s;
}

/* The local VLAs cannot alias the global-reached g.data; no runtime
   alias checks may be required.  */
/* { dg-final { scan-tree-dump-not "versioning for alias required" "vect" } } */
