/* PR tree-optimization/46233 */
/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dce" } */

int
foo ()
{
  int i = 0;
  while (1)
    i += foo ();
}
