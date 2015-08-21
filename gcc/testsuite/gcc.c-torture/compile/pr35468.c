/* PR tree-optimization/35468 */
/* { dg-do compile } */

void
foo (void)
{
  *(char *) "c" = 'x';
}
