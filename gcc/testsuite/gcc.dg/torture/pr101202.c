/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize" } */

int printf(const char *, ...);
unsigned a, b, d;
int c, e, f;
int main()
{
  while (a)
    if (b)
      {
	f = a;
	while (e)
	  {
	    int h, i;
	    if (d)
	      {
		h = a;
		i = d;
L:
		d = a | d && c;
		if (a)
		  {
		    printf("%d", a);
		    goto L;
		  }
	      }
	    a = h;
	    d = i;
	  }
      }
  return 0;
}
