/* { dg-do compile } */

void foo (int *, int *);
int bar ()
{
  int a = 0;
  int b = 0;
  if (b != 0)
    {
      int ax = a;
      int bx = b;
      while (bx != 0)
	{
	  int tem = ax % bx;
	  ax = bx;
	  bx = tem;
	}
    }
  foo (&a, &b);
}
