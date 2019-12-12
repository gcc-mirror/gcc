/* { dg-do compile { target int128 } } */
/* { dg-options "-O -ftree-parallelize-loops=2 -fno-tree-dominator-opts --param parloops-min-per-thread=30" } */

void
zf (__int128 ct)
{
  __int128 *rk = &ct;

  if (0)
    {
      int jj;

t9:
      for (jj = 0; jj < 60; ++jj)
	{
	}

      __builtin_unreachable ();
    }

  while (*rk < 1)
    ++*rk;

  goto t9;
}
