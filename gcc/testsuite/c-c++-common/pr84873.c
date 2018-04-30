/* { dg-do compile } */
/* { dg-additional-options "-frounding-math" } */

int
i1 (int w3, int n9)
{
  return w3 >> ((long int)(1 + 0.1) + -!n9);
}
