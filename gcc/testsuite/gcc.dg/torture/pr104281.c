/* PR tree-optimization/104281 */
/* { dg-do run } */

unsigned a = 1;
int b, c = 2;
long d;

int
main ()
{
  while (1)
    {
      int m = a;
    L:
      a = ~(-(m || b & d));
      b = ((1 ^ a) / c);
      if (b)
	goto L;
      break;
    }
  return 0;
}
