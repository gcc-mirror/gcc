/* { dg-do compile} */
/* { dg-options "-fsanitize=integer-divide-by-zero" } */

void
foo (void)
{
  int A[-2 / -1] = {};
}
