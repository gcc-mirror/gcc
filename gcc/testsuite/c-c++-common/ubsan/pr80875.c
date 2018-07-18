/* PR sanitizer/80875 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int
foo (void)
{
  return ~__INT_MAX__ * (0 / 0); /* { dg-warning "division by zero" } */
}
