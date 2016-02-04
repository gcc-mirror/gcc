/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c, d;
int e[100];
void
fn1 ()
{
  int *f = &d;
  c = 6;
  for (; c; c--)
    {
      b = 0;
      for (; b <= 5; b++)
	{
	  short g = e[(b + 2) * 9 + c];
	  *f = *f == a && e[(b + 2) * 9 + c];
	}
    }
}
