/* PR tree-optimization/101159 */

unsigned long a;
long b;

void
foo (void)
{
  a += __builtin_popcountl (b);
}
