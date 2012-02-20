/* PR tree-optimization/52286 */

extern void abort (void);

int
main ()
{
#if __SIZEOF_INT__ > 2
  int a, b;
  asm ("" : "=r" (a) : "0" (0));
  b = (~a | 1) & -2038094497;
#else
  long a, b;
  asm ("" : "=r" (a) : "0" (0));
  b = (~a | 1) & -2038094497L;
#endif
  if (b >= 0)
    abort ();
  return 0;
}
