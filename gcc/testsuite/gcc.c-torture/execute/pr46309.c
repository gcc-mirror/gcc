/* PR tree-optimization/46309 */

extern void abort (void);

unsigned int *q;

__attribute__((noinline, noclone)) void
bar (unsigned int *p)
{
  if (*p != 2 && *p != 3)
    (!(!(*q & 263) || *p != 1)) ? abort () : 0;
}

int
main ()
{
  unsigned int x, y;
  asm volatile ("" : : : "memory");
  x = 2;
  bar (&x);
  x = 3;
  bar (&x);
  y = 1;
  x = 0;
  q = &y;
  bar (&x);
  y = 0;
  x = 1;
  bar (&x);
  return 0;
}
