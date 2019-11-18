/* Test C2x attribute syntax.  Valid use of fallthrough attribute.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wextra" } */

int
f (int a)
{
  int b = 2;
  switch (a)
    {
    case 1:
      b = 1; /* { dg-warning "may fall through" } */
    case 2:
      b = 2;
      [[fallthrough]];
    case 3:
      b += 7;
      break;
    }
  return b;
}
