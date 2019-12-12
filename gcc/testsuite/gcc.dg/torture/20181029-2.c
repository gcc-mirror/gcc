/* { dg-do compile } */

int a, b;
unsigned long c;
unsigned long *d;
void e();
void f()
{
  if (c)
    {
      if (a)
	{
	  e();
	  d[0] = c;
	  d[1] = b;
	}
      b = c;
    }
}
