/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

extern void vrp_keep (void);
extern void vrp_kill (void);

void
f2 (int s, int b)
{
  if (s > 4)
    s = 4;
  if (s < -16)
    s = -16;
  /* s in [-16, 4].   */
  b = (b & 1) + 1;
  /* b in range [1, 2].  */
  b = s << b;
  /* b in range [-64, 16].  */
  if (b == -2)
    vrp_keep ();
  if (b <= -65)
    vrp_kill ();
  if (b >= 17)
    vrp_kill ();
}

/* { dg-final { scan-tree-dump-times "vrp_keep \\(" 1 "evrp"} } */
/* { dg-final { scan-tree-dump-times "vrp_kill \\(" 0 "evrp"} } */
