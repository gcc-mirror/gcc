/* { dg-do run } */

int a, b[4], c, d, e, f;
int main()
{
  a--;
  for (f = 3; f >= 0; f--)
    {
      for (e = 0; e < 4; e++)
	c = 0;
      for (; c < 4; c++)
	{
	  d = f && a > 0 && f > (2147483647 - a) ? 0 : b[f];
	  continue;
	}
    }
  return 0;
}
