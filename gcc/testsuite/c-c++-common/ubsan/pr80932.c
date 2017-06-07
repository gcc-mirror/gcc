/* PR sanitizer/80932 */
/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int x = 1;

long int
foo (void)
{
  return ((long) (13801962912760474560ULL * x) - (long) (15334142073106273231ULL * x)) * -6;
}

int
main ()
{
  foo ();
}
