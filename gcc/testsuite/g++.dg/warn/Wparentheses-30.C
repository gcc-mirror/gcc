/* PR c/71926 */
/* { dg-options "-Wparentheses" }  */

int
f (void)
{
  int a = 1, b = 2, c = 3, d = 4;
  if (a = 2 || (b != 3 && c != 4 && d != 5)) /* { dg-warning "9:suggest parentheses" } */
    return 1;
  return 0;
}
