/* { dg-do compile } */

unsigned a;
int b, c;

void f(void)
{
  if(a)
    {
      for(a = 0; a < 2; a++)
	a /= 7;

      for(;; a++)
	{
	  if(a)
	    lbl1:
		b++;

	  if(c)
	    goto lbl1;
lbl2:
	  ;
	}
    }

  goto lbl2;
}
