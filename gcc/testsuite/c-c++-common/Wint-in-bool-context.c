/* PR c++/77434 */
/* { dg-options "-Wint-in-bool-context" } */
/* { dg-do compile } */

int foo (int a, int b)
{
  if (a > 0 && a <= (b == 1) ? 1 : 2) /* { dg-warning "boolean context" } */
    return 1;

  if (a > 0 && a <= (b == 2) ? 1 : 1) /* { dg-bogus "boolean context" } */
    return 2;

  if (a > 0 && a <= (b == 3) ? 0 : 2) /* { dg-warning "boolean context" } */
    return 3;

  if (a == b ? 0 : 0) /* { dg-bogus "boolean context" } */
    return 4;

  if (a == ((b ? 2|4 : 1) & 3 ? 0 : 2)) /* { dg-bogus "boolean context" } */
    return 5;

  if (a ? 1 : 1+1) /* { dg-warning "boolean context" } */
    return 6;

  if (b ? 1+1 : 1) /* { dg-warning "boolean context" } */
    return 7;

  for (a = 0; 1 << a; a++); /* { dg-warning "boolean context" } */

  return 0;
}
