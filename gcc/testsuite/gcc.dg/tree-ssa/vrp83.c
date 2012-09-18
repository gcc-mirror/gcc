/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

extern void vrp_keep (void);

void
f2 (int c, int b)
{
  int s = 0;
  if (c == 0)
    s += 1;
  else if (c < 1)
    s -= 1;
  /* s in [-1, 1].   */
  b = (b & 1) + 1;
  /* b in range [1, 2].  */
  b = s << b;
  /* b in range [-4, 4].  */
  if (b == -4)
    vrp_keep ();
  if (b == 4)
    vrp_keep ();
}

void
f3 (int s, int b)
{
  if (s >> 3 == -2)
    {
      /* s in range [-16, -9].  */
      b = (b & 1) + 1;
      /* b in range [1, 2].  */
      b =  s << b;
      /* b in range [bmin << smax, bmax << smin],
                    == [-16 << 2, -9 << 1]
                    == [-64, -18].  */
      if (b == -64)
	vrp_keep ();
      if (b == -18)
	vrp_keep ();
    }
}

void
f4 (unsigned int s, unsigned int b)
{
  s |= ~(0xffU);
  /* s in [0xffffff00, 0xffffffff].  */
  b = (b & 1) + 1;
  /* b in [1, 2].  */
  b = s << b;
  /* s in [0xfffffc00, 0xfffffffe].  */
  if (b == ~0x3ffU)
    vrp_keep ();
  if (b == ~0x1U)
    vrp_keep ();
}

/* { dg-final { scan-tree-dump-times "vrp_keep \\(" 6 "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
