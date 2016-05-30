/* { dg-do link } */
/* { dg-options " -O1 -floop-nest-optimize" } */
/* { dg-additional-options "-flto" { target lto } } */

int a1, c1, cr, kt;
int aa[2];

int
ce (void)
{
  while (a1 < 1)
    {
      int g8;
      for (g8 = 0; g8 < 3; ++g8)
	if (c1 != 0)
	  cr = aa[a1 * 2] = kt;
      for (c1 = 0; c1 < 2; ++c1)
	aa[c1] = cr;
      ++a1;
    }
  return 0;
}

int
main (void)
{
  return ce ();
}
