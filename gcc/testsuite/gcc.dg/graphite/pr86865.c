/* { dg-do run } */
/* { dg-options "-O2 -fgraphite-identity -fstack-reuse=none -fwrapv -fno-tree-ch -fno-tree-dce -fno-tree-dominator-opts -fno-tree-loop-ivcanon" } */

int xy, tb;

void
bt (void)
{
  for (xy = 0; xy >= 0; --xy)
    {
      int yt[8] = { 0 };
      int pz[2] = { 0 };
      int sa[32] = { 0 };
      int us;

      for (us = 0; us < 8; ++us)
	yt[us] = 0;

      (void) yt;
      (void) pz;
      (void) sa;
    }

  tb = 1;
}

int
main (void)
{
  bt ();
  if (xy != -1)
    __builtin_abort ();

  return 0;
}
