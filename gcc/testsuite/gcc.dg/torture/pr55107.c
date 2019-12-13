/* { dg-do compile } */
/* { dg-additional-options "-fno-split-loops" } */

typedef unsigned short uint16_t;

uint16_t a, b;

uint16_t f(void)
{
  int c, **p;
  short d = 2, e = 4;

  for (;; b++)
    {
      int *j, k = 0;

      for (; *j; j++)
	{
	  for(; c; c++)
	    for(; k < 1; k++)
	      {
		short *f = &d;

		if(b)
		  return *f;
	      }
	}

      if(!c)
	d *= e;

      ((a = d) ? b = 0 : (**p ? : 1) != (d != 1 ? : (a = 0))) != (k ? a : 0)
	  < (a *= c = k) && (**p = 0);
    }
}
