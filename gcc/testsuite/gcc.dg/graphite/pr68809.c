/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

int ae, vs;
char ue;

void
kc (char);

void
pm (void)
{
  unsigned int v9;
  int gf = 0;
  vs = 1;
  while (vs)
    {
      gf -= ue;
      kc (ue);
      for (ae = 0; ae < 70; ++ae)
	{
	}
      ae &= 4;
      ae ^ (gf != 0) && ((ue = 0) != 0);
    }
  v9 = ue + 1;
  ue - v9 && ((ue = 0) != 0);
}
