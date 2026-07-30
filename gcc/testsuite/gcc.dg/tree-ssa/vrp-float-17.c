/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fdump-tree-evrp" } */

extern void link_error (void);

/* [-Inf, 2.0] U [4.0, +Inf], meet [1.0, 5.0] -> [1.0, 2.0] U [4.0, 5.0].  */
void
meet_ineq (double x)
{
  if (x <= 2.0 || x >= 4.0)
    if (x >= 1.0 && x <= 5.0)
      if (x == 3.0)
	link_error ();
}

/* [0.0, 5.0] U [10.0, 15.0], meet [4.0, 11.0] -> [4.0, 5.0] U [10.0, 11.0].  */
void
meet_intervals (double x)
{
  if ((x >= 0.0 && x <= 5.0) || (x >= 10.0 && x <= 15.0))
    if (x >= 4.0 && x <= 11.0)
      if (x == 7.0)
	link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
