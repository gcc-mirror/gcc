/* PR tree-optimization/98255 */
/* { dg-do run } */
/* { dg-options "-Os" } */
/* { dg-additional-options "-fPIC" { target fpic } } */

struct A { volatile unsigned b; unsigned c; };
int d, *e, h, k, l;
static struct A f;
long g;
static unsigned i = -2U;
volatile int j;

long
foo (void)
{
  char n[4][4][3]
    = { { {9, 2, 8}, {9, 2, 8}, {9, 2, 8}, {9} }, { {8} }, { {8} }, { {2} } };
  while (d)
    {
      for (; f.c < 4; f.c++)
	{
	  *e = 0;
	  h = n[f.c + 4][0][d];
	}
      while (g)
	return n[0][3][i];
      while (1)
	{
	  if (k)
	    {
	      j = 0;
	      if (j)
		continue;
	    }
	  if (l)
	    break;
	}
    }
  return 0;
}

int
main ()
{
  asm volatile ("" : "+g" (d), "+g" (g), "+g" (f.c));
  asm volatile ("" : "+g" (e), "+g" (k), "+g" (l));
  foo ();
  return 0;
}
