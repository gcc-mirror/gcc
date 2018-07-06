/* { dg-do compile } */
/* { dg-options "-O -floop-nest-optimize -fno-tree-loop-im" } */

int mg, et;

void
s5 (int is)
{
  if (is == 0)
    {
g6:
      ++is;
    }

  while (mg < 1)
    {
      while (et < 1)
	{
	  if (is == 0)
	    return;

	  ++et;
	}

      while (mg < 1)
	++mg;
    }

  goto g6;
}

