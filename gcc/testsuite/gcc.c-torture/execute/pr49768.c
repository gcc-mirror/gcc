/* PR tree-optimization/49768 */

extern void abort (void);

int
main ()
{
  static struct { unsigned int : 1; unsigned int s : 1; } s = { .s = 1 };
  if (s.s != 1)
    abort ();
  return 0;
}
