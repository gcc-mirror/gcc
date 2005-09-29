/* PR target/22576 */
/* Testcase reduced by Volker Reichelt */
/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

int
foo (long double d)
{
  return d == 0;
}
