/* { dg-do compile } */
/* { dg-options "-O3 -ftree-parallelize-loops=2 -fipa-pta" } */

int a, b;
int *d;
void f(void)
{
  int c;
  b %= 1;

  if(1 - (b < 1))
    {
      int *q = 0;

      if(a)
	{
	  c = 0;
lbl:
	  for(*d; *d; ++*d)
	    if(c ? : a ? : (c = 1) ? : 0)
	      *q &= 1;
	  return;
	}

      q = (int *)1;
    }
  goto lbl;
}

