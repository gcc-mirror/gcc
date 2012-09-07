/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-switch-conversion -fdump-tree-vrp1" } */

extern void vrp_keep (void);

/* Test <<.  */

void
f3 (int s, int b)
{
  if (s >> 3 == -2)
    /* s in range [-16, -9].  */
    {
      s += 17;
      /* s in range [1, 8].  */
      b = (b & 1) + 1;
      /* b in range [1, 2].  */
      b =  b << s;
      /* b in range [bmin << smin, bmax << smax],
                    == [1 << 1, 2 << 8]
                    == [2, 512].  */
      if (b == 2)
	vrp_keep ();
      if (b == 512)
	vrp_keep ();
    }
}

int
main ()
{
  return 0;
}

/* { dg-final { scan-tree-dump-times "vrp_keep \\(" 2 "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */


