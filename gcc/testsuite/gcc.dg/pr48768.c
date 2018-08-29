/* PR debug/48768 */
/* { dg-do compile } */
/* { dg-options "-O -fcompare-debug" } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } } */

int a, b;

int
bar (void)
{
  int i, j = 1;
  for (i = 0; i != 10; i++)
    {
    lab:
      if (i)
	{
	  int *k = &j;
	}
      else if (j)
	goto lab;
    }
  return 1;
}

inline int
foo (int x)
{
  unsigned int c = x;
  int d = x;
  if (bar ())
    for (; c; c++)
      while (x >= 0)
	if (foo (d) >= 0)
	  {
	    d = bar ();
	    a = b ? b : 1;
	  }
  return 0;
}
