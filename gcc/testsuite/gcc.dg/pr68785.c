/* PR tree-optimization/68785 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

int
foo (void)
{
  return *(int *) "";
}
