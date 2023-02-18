/* PR tree-optimization/108657 */
/* { dg-do run } */
/* { dg-options "-O3 -ftrivial-auto-var-init=zero" } */

int c, e, f;
static int *d = &c;

__attribute__((noipa)) void
foo (void)
{
  if (c != 1)
    __builtin_abort ();
}

int
main ()
{
  for (c = 1; c >= 0; c--)
    {
      e = 0;
      for (int j = 0; j <= 2; j++)
	{
	  short k[1];
	  if (e)
	    break;
	  e ^= f;
	}
    }
  *d = 1;
  foo ();
}
