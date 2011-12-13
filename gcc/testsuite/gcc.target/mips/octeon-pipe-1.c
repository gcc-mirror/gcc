/* Check that we use the octeon pipeline description.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=octeon -fdump-rtl-sched2" } */

NOMIPS16 int f (int a, int b)
{
  return a / b;
}

/* { dg-final { scan-rtl-dump "octeon_mult\\*71" "sched2" } }  */
/* { dg-final { cleanup-tree-dump "sched2" } }  */
