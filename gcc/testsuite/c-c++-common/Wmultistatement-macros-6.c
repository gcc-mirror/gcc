/* PR c/80116 */
/* { dg-options "-Wmultistatement-macros" } */
/* { dg-do compile } */

#define M \
  if (x) x++; x++

void
f (int x)
{
  M;
  M;
  M;
  M;
  M;
  M;
  M;
  M;
  M;
  M;
  M;
}
