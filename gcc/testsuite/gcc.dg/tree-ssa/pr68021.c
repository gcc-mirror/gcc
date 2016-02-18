/* { dg-do compile } */
/* { dg-options "-O3" } */

char a;
void fn1 (char *p1, int p2, int p3)
{
  int i, x;
  for (i = 0; i < 10; i++)
    {
      for (x = 0; x < p3; x++)
	{
	  *p1 = a;
	  p1--;
	}
      p1 += p2;
    }
}
