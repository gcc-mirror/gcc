/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

extern void vrp_keep (void);

void
f2 (int s, int b)
{
  if (s > 1)
    s = 1;
  /* s in [minint, 1].   */
  b = (b & 1) + 1;
  /* b in range [1, 2].  */
  b = s << b;
  /* b in range [minint+4, maxint-3].  */
  if (b == -2)
    vrp_keep ();
}

/* { dg-final { scan-tree-dump-times "vrp_keep \\(" 1 "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
