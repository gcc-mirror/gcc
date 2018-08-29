/* PR bootstrap/83396 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

int bar (int);
int baz (int);

int
foo (int x)
{
  return bar (x) || baz (x) != 0;
}
