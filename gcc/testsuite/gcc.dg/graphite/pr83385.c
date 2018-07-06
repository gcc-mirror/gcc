/* { dg-do run } */
/* { dg-options "-O1 -floop-nest-optimize" } */

int xc, n1 = 0;
int bx[2];

int
main (void)
{
  int aj = 1;
  int cs;

  for (cs = aj; cs >= 0; --cs)
    {
      int sq;

      for (sq = 0; sq < 2; ++sq)
	{
	  if (aj != 0)
	    --n1;

	  for (xc = 0; xc < 2; ++xc)
	    bx[xc] = 0;
	}

      --aj;
    }

  if (n1 != -2)
    __builtin_abort ();
  return 0;
}
