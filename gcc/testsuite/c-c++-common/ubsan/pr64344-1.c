/* PR sanitizer/64344 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=float-cast-overflow" } */

int
foo (float x)
{
  return __builtin_log ((double ) x);
}
