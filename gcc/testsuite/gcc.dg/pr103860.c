/* PR rtl-optimization/103860 */
/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

static int d, *e;
int f;

__attribute__((noinline)) signed char
foo (signed char b, signed char c)
{
  return b + c;
}

int
main ()
{
  signed char l;
  for (l = -1; l; l = foo (l, 1))
    {
      while (d < 0)
	;
      if (d > 0)
	{
	  f = 0;
	  *e = 0;
	}
    }
  d = 0;
  return 0;
}
