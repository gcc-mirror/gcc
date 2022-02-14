/* PR tree-optimization/104499 */

typedef int __attribute__((__vector_size__ (8 * sizeof (int)))) V;

V v;

void
foo (void)
{
  v = ((1 | v) != 1);
}
