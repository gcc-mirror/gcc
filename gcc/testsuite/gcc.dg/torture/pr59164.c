/* { dg-do compile } */

int a, d, e;
long b[10];
int c[10][8];

int fn1(p1)
{ 
  return 1 >> p1; 
}

void fn2(void)
{
  int f;
  for (a=1; a <= 4; a++)
    {
      f = fn1(0 < c[a][0]);
      if (f || d)
	e = b[a] = 1;
    }
}
