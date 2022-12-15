/* { dg-do compile } */
/* { dg-additional-options "-w" } */

short a;
long b;
void fn1()
{
  int c = a = 1;
  for (; a; a++)
    {
      for (; 9 <= 8;)
	for (;;) {
	    a = 20;
	    for (; a <= 35; a++)
	      ;
line:;
	}
      if ((c += 264487869) == 9)
	{
	  unsigned *d = 0;
	  for (; b;)
	    d = (unsigned *)&c;
	  if (d)
	    for (;;)
	      ;
	}
    }
  goto line;
}
