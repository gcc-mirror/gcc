/* PR 23497 */
/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

typedef _Complex float C;
C foo()
{
  C f;
  __real__ f = 0;
  __imag__ f = 0;
  return f;
}
