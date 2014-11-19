/* PR sanitizer/63879 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int a;
void
fn1 ()
{
  int b = 2;
  for (; a;)
    while (b >= 0)
      b--;
}
