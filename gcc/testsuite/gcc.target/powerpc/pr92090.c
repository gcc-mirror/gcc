/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -Os" } */
/* { dg-additional-options "-mbig" { target powerpc64le-*-* } } */

/* Verify that we don't ICE.  */

_Atomic int a;
_Atomic long double b, c;
int j;
void foo (void);
void bar (int, int, int, int);

void
bug (void)
{
  b = 1;
  int d, e, f, g;
  while (a)
    ;
  for (int h = 0; h < 10000; h++)
    {
      double i = b /= 3;
      foo ();
      if (i)
	{
	  if (i == 1)
	    d++;
	  e++;
	  b = 0;
	}
      else
	{
	  if (i == 2)
	    f++;
	  g++;
	  b = 1;
	}
    }
  bar (d, e, f, g);
  c = 1;
  for (int h; h; h++)
    j = 0;
}
