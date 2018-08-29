/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int x3, za;
int hg[1];

void
yw (int dq)
{
  const int r7 = 2;

  while (dq < 1)
    {
      for (x3 = 0; x3 < r7; ++x3)
	for (za = 0; za < r7; ++za)
	  hg[1] = 0;
      ++dq;
    }

  x3 = 0;
  while (x3 < r7)
    {
      ++x3;
      if (x3 == 0)
	break;
    }
}
