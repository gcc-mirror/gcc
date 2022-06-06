/* PR tree-optimization/105770 */
/* { dg-do compile } */
/* { dg-options "-O1 -funswitch-loops -fno-tree-forwprop" } */

char a;

void
foo (void)
{
  while (a)
    switch (a)
      {
      case ' ':
      case '\t':
	return;
      }

  __builtin_unreachable ();
}
