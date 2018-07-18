/* Verify the lower and upper bounds of floating directives with
   precision whose range crosses zero.
  { do-do compile }
  { dg-options "-O2 -Wall -fdump-tree-optimized" } */

static const double x = 1.23456789;

/* All calls to failure_range must be eliminated.  */
extern void failure_range (int, int, int);

/* All calls to verify_{lo,hi}_bound must be retained.  */
extern void verify_lo_bound (int, int);
extern void verify_hi_bound (int, int);

int test_a (int p)
{
  if (p < -1 || 3 < p)
    p = -1;

  int n = __builtin_snprintf (0, 0, "%.*A", p, x);
  if (n < 6 || 25 < n)
    failure_range ('A', 6, 25);

  if (n == 6) verify_lo_bound ('A', 6);
  if (n == 25) verify_hi_bound ('A', 25);

  return n;
}

int test_e (int p)
{
  if (p < -1 || 3 < p)
    p = -1;

  int n = __builtin_snprintf (0, 0, "%.*E", p, x);
  if (n < 5 || 17 < n)
    failure_range ('E', 5, 17);

  if (n == 5) verify_lo_bound ('E', 5);
  if (n == 17) verify_hi_bound ('E', 17);

  return n;
}

int test_f (int p)
{
  if (p < -1 || 3 < p)
    p = -1;

  int n = __builtin_snprintf (0, 0, "%.*F", p, x);
  if (n < 1 || 13 < n)
    failure_range ('F', 1, 13);

  if (n == 1) verify_lo_bound ('F', 1);
  if (n == 13) verify_hi_bound ('F', 13);

  return n;
}

int test_g (int p)
{
  if (p < -1 || 3 < p)
    p = -1;

  int n = __builtin_snprintf (0, 0, "%.*G", p, x);
  if (n < 1 || 12 < n)
    failure_range ('G', 1, 12);

  if (n == 1) verify_lo_bound ('G', 1);
  if (n == 12) verify_hi_bound ('G', 12);

  return n;
}

/* { dg-final { scan-tree-dump-times "snprintf" 4 "optimized"} }
   { dg-final { scan-tree-dump-not "failure_range" "optimized"} }
   { dg-final { scan-tree-dump-times "verify_" 8 "optimized"} } */
