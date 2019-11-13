/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -Os -w" } */
/* { dg-additional-options "-mbig" { target powerpc64le-*-* } } */

/* Verify that we don't ICE.  */

int a;
static _Atomic long double b, c, d, m;
double n;
extern int foo (void);
extern void bar (int, int, int, int);

void
bug (void)
{
  b = 1.79769313486231580793728971405301199e308L;
  for (int i = 0; i < 10000; i++)
    if (__builtin_isinf (n))
      b;
  c = 1;
  int e, f, g, h;
  while (a)
    ;
  for (int i; i; i++)
    {
      double j = c /= foo ();
      if (__builtin_isinf (j))
	{
	  if (foo == 1 << 31)
	    e++;
	  f++;
	  c = 0;
	}
      else
	{
	  if (foo == 1 << 30)
	    g++;
	  h++;
	  c = 1;
	}
    }
  bar (e, f, g, h);
  d = 1.79769313486231580793728971405301199e308L;
  m = 1;
}
