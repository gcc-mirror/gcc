/* { dg-do run } */
/* { dg-options "-O -floop-nest-optimize -fno-tree-scev-cprop -fdump-tree-graphite-details" } */

int rx, in;

int
main (void)
{
  const int tj = 3;
  int as[tj];
  static int l4;

  while (l4 < 1)
    {
      for (rx = 0; rx < tj; ++rx)
	{
	  for (in = 0; in < tj; ++in)
	    as[in] = 1;
	  as[rx] = 0;
	}
      ++l4;
    }

  if (as[tj - 1] != 0)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump "loop nest optimized" "graphite" } } */
