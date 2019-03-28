/* { dg-do compile } */

int x2;

void
m2 (void)
{
  goto gg;

  int fz, vh = 0;

  for (fz = 0; fz < 1; ++fz)
    {
      vh ^= x2;

      if (0)
	{
gg:
	  x2 %= 1;
	  x2 += vh;
	}
    }
}
