/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int mc[2];
int f2, sk;
short int hm;

void
zm (void)
{
  int k1;

  for (k1 = 0; k1 < 2; ++k1)
    {
      for (sk = 0; sk < 2; ++sk)
	mc[sk] = hm = ++f2;
      if (hm >= 0)
	++hm;
    }
}
