/* PR c++/44062 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void
f ()
{
  int i = 4;
  static_cast <void> (i);
  int j;
  j = 5;
  static_cast <void> (j);
}
