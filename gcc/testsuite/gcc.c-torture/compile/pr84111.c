/* PR tree-optimization/84111 */

void
foo (int x, int y, int z)
{
  int a = 0;
  int *b = &x;

  while (a < 1)
    {
      int c = y;
      *b = x;
 lab:
      for (a = 0; a < 36; ++a)
        {
          *b = 0;
          if (x != 0)
            y = 0;
          while (c < 1)
	    ;
        }
    }
  if (z < 33)
    {
      b = (int *) 0;
      ++y;
      ++z;
      if (x / *b != 0)
        goto lab;
    }
}
