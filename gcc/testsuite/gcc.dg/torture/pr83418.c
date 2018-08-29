/* { dg-do compile } */

void
yj (int j4)
{
  int t3;

  for (t3 = 0; t3 < 6; ++t3)
    {
      short int v4 = t3;

      if (v4 == j4 || v4 > t3)
	for (;;)
	  {
	  }
    }
}
