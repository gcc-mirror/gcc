/* PR tree-optimization/46461 */

void
foo (char *c)
{
  c[7] = 0xff;
  __builtin_memset (c + 8, 0xff, 8);
}
