/* PR tree-optimization/86526 */

void
foo (char *x)
{
  if (__builtin_memcmp (x, "\0a", 3))
    __builtin_abort ();
}
