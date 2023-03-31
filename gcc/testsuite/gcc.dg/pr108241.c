/* PR driver/108241 */
/* { dg-options "-Os -frounding-math -fvar-tracking-assignments -fno-dce -fno-trapping-math -fno-tree-dce -fno-tree-dse" } */

long int n1;
int n2, n3, n4;
char n5;

void
foo (long int x1, long int x2, int x3, int x4, int x5, char x6, char x7)
{
  char a01 = n2, a02 = x4, a03 = 0;
  short int a04;
  unsigned short int a05 = x5;
  int a06, a07, a08 = a05, a09 = x3, a10 = 0;
  long int a11, a12 = x4;

  if (x1)
    {
      a07 = x6 + (float)0x1000001;
      a03 = a12 = a01 = a06 = ~0;

      if (x5)
	a11 = n5;
    }
  else
    {
      a10 = x3 = n3;
      if (n3)
	a06 = a05 = x7;
    }

  if (n3 < n5)
    {
      n4 = (x2 == x4) + !n1;
      if (n4 % (n1 % x3))
	{
	  a04 = n4;
	  a02 = n2;
	}

      if (x3)
	{
	  a05 = !n1 % n2;
	  a08 = n1;
	  a04 = x5 + a06;
	}

      if (a12)
	a09 = n3 + n4;

      a12 = a07;
      n3 = a11 % x1;
      n5 += x6;
      n1 = a04;
    }

  n4 = x2 % x5 % a11;
  a06 = a10 + a08 % a02 == n4;
  a09 = a09 == a01 * x7;
  n4 = x4;
  a12 += x4 / 0xc000000000000000 + !a03;
  a03 = !a05;
}
