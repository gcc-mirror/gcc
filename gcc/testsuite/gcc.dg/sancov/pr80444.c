/* PR sanitizer/80444 */
/* { dg-do compile } */
/* { dg-options "-fsanitize-coverage=trace-pc -fcompare-debug" } */

void
foo (void)
{
  int a = 0;
}
