/* { dg-do run } */

int a, b, c, d, e;
int main()
{
  int f = 0, g = 0;
  for (; f < 2; f++)
    {
      int h, i;
      for (h = 0; h < 2; h++)
	{
	  b = e = g ? a % g : 0;
	  c = d;
	  for (i = 0; i < 1; i++)
	    g = 0;
	  for (; g < 2; g++)
	    ;
	}
    }
  return 0;
}
