/* { dg-do compile } */
/* { dg-options "-O2 -w -ftree-vectorize -mavx2" } */

int cn;
int *li;

void
y8 (void)
{
  int gv;
  int *be = &gv;
  short int v4 = 2;

  while (*li != 0)
    {
      int sy;
      for (sy = 0; sy < 5; ++sy)
	{
	  int **t6 = &be;
	  gv |= sy ? 0 : v4;
	  if (gv != 0)
	    ++gv;
	  t6 = (int **) &cn;
	  if (gv != 0)
	    *t6 = 0;
	}
      for (gv = 0; gv < 24; ++gv)
	v4 |= 1 <= 1 % 0;
      ++(*li);
    }
}
