/* PR sanitizer/63866 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -fdump-ipa-cgraph" } */

int
foo (int x, int y)
{
  return x + y;
}

