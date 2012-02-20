/* PR tree-optimization/52286 */

extern void abort (void);

int
main ()
{
  int a, b;
  asm ("" : "=r" (a) : "0" (0));
  b = (~a | 1) & -2038094497;
  if (b >= 0)
    abort ();
  return 0;
}
