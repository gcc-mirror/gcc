/* PR tree-optimization/90671 */
/* { dg-do compile } */
/* { dg-additional-options "-w -g" } */

int a;

int
main ()
{
  int b, c;
  for (c = 0; c < 2; c++)
    while (a)
      if (b)
	break;
  return 0;
}
