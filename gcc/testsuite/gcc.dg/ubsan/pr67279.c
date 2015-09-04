/* PR sanitizer/67279 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -w" } */

#define INT_MIN (-__INT_MAX__ - 1)

void
foo (void)
{
  static int a1 = 1 << 31;
  static int a2 = 10 << 30;
  static int a3 = 100 << 28;
  static int a4 = INT_MIN / -1;
}
