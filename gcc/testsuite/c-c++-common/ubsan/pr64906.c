/* PR sanitizer/64906 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=integer-divide-by-zero -O -Werror=maybe-uninitialized" } */

int
fn1 (int f, int s)
{
  int ret = 0;
  if (f)
    ret = s / (f ? (unsigned long) 8 : 0);
  return ret;
}
