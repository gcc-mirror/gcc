/* PR rtl-optimization/108263 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int v, *p;

void
foo (void)
{
  int i;
  for (i = 0; ; i++)
    {
      if (v)
	{
	  __label__ l1;
	  asm goto ("" : : : : l1);
	l1:
	  return;
	}
      if (p[i])
	break;
    }
  asm goto ("" : : "r" (i) : : l2);
l2:;
}
