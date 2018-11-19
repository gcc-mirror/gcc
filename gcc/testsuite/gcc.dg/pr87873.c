/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-vectorize" } */

long k3;
int gs;

void
s2 (int aj)
{
  while (aj < 1)
    {
      gs ^= 1;
      k3 = (long) gs * 2;
      if (k3 != 0)
	k3 = 0;

      ++aj;
    }
}
