/* PR tree-optimization/53748 */

typedef unsigned int V __attribute__ ((__vector_size__ (sizeof (int) * 4)));

void
foo (int x, V *y)
{
  *y = x ? ((V) { ~0U, ~0U, ~0U, ~0U }) : ((V) { 0, 0, 0, 0 });
}
