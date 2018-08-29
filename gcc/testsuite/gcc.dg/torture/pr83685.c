/* { dg-do compile }  */

int _setjmp (void *);
void foo (int);

void
bar (int e, int b, char c, void *d)
{
  while (b)
    {
      if (_setjmp (d))
	foo (e);
      if (c)
	{
	  e--;
	  foo (0);
	}
      e++;
    }
}
