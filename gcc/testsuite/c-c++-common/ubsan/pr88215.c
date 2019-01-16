/* PR c++/88215 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=integer-divide-by-zero" } */

int
foo (void)
{
  int a = 2, __attribute__ ((__unused__)) b = 1;
  int f = a / b;
  return f;
}
