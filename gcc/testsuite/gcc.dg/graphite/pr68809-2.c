/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int ae, vs, gf;
char ue;

void
kc (char);

void
pm (void)
{
  unsigned int v9;
  int td = (gf != 0);
  while (vs)
    {
      kc (ue);
      for (ae = 0; ae < 70; ++ae)
	{
	}
      ae &= 4;
      ae ^ td && ((ue = 0) != 0);
      ++vs;
    }
  v9 = ue + 1;
  ue - v9 && ((ue = 0) != 0);
}
