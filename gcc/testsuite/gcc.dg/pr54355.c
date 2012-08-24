/* PR c/54355 */
/* { dg-do compile } */

void
foo (int i)
{
  switch (i)
  {
  case 0: T x > /* { dg-error "(label|unknown type|expected)" } */
  }
} /* { dg-error "expected" } */
