/* { dg-do compile } */
/* { dg-options "-O -floop-nest-optimize -ftree-pre -fno-tree-loop-im" } */

long long unsigned int od;
int zj, fk, ea;

void
ke (void)
{
  if (od != 0 && zj != 0)
    {
      for (fk = 0; fk < 2; ++fk)
	{
	}

      if (od == (long long unsigned int) zj)
	zj = 0;

      for (ea = 0; ea < 2; ++ea)
	{
	}
    }
}
