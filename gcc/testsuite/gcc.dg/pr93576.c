/* PR c/93576 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  int b[] = { 0 };
  (char (*)[(1, 7, 2)]) 0;
}
