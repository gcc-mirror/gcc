/* PR tree-optimization/35468 */

void
foo (void)
{
  *(char *) "c" = 'x';
}
