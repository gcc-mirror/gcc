/* PR sanitizer/80536 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int
foo (int i)
{
  return ((i * (unsigned long long) (-0 + 1UL)) * 2) % 1;
}
