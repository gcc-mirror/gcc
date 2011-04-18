/* PR rtl-optimization/47899 */
/* { dg-do compile } */
/* { dg-options "-O -funroll-loops" } */

extern unsigned int a, b, c;
extern int d;

static int
foo (void)
{
lab:
  if (b)
    for (d = 0; d >= 0; d--)
      if (a || c)
	for (; c; c++)
	  ;
      else
	goto lab;
}

int
main ()
{
  foo ();
  return 0;
}
