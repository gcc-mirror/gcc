/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fdump-tree-evrp" } */

extern void link_error (void);

/* x is [-Inf, 2.0] U [4.0, +Inf]; 3.0 falls in the gap.  */
void
ineq_gap (double x)
{
  if (x <= 2.0 || x >= 4.0)
    if (x == 3.0)
      link_error ();
}

/* x is [2.0, 2.0] U [4.0, 4.0]; 3.0 falls in the gap.  */
void
two_points (double x)
{
  if (x == 2.0 || x == 4.0)
    if (x == 3.0)
      link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
