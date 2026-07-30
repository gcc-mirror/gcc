/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fdump-tree-evrp-details" } */

/* Verify the pretty-printer shows two-piece ranges correctly.  */

/* Two disjoint points: {2.0} U {4.0}.  */
double
pp_two_points (int c)
{
  double x;
  if (c)
    x = 2.0;
  else
    x = 4.0;
  return x;
}

/* Two intervals from inequalities: [-Inf, 2.0] U [4.0, +Inf].  */
double
pp_ineq_gap (double x)
{
  if (x <= 2.0 || x >= 4.0)
    return x;
  return 0.0;
}

/* {2.0, 2.0}{4.0, 4.0} -- two disjoint pieces on one line.  */
/* { dg-final { scan-tree-dump "2\\.0e\\+0\[^\r\n\]*2\\.0e\\+0\[^\r\n\]*4\\.0e\\+0\[^\r\n\]*4\\.0e\\+0" "evrp" } } */
/* [-Inf, 2.0][4.0, +Inf] -- inequality gap on one line.  */
/* { dg-final { scan-tree-dump "-Inf, 2\\.0e\\+0\[^\r\n\]*4\\.0e\\+0\[^\r\n\]*\\+Inf" "evrp" } } */
