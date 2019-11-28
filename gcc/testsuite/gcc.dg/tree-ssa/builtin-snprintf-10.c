/* PR tree-optimization/92691 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (int x, char *y)
{
  if (x != 0)
    __builtin_snprintf (y, 0, "foo");
}
