/* { dg-do compile } */
/* { dg-additional-options "-O -ftrapv" } */

int
k5 (int u5, int aw)
{
  int v6;

  while (u5 < 1)
    {
      while (v6 < 4)
	++v6;

      v6 = 0;
      aw += u5 > 0;
      ++u5;
    }

  return aw;
}
