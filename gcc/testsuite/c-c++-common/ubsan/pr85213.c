/* PR sanitizer/85213 */
/* { dg-do compile } */
/* { dg-options "-O1 -fsanitize=undefined -fcompare-debug" } */

int
foo (int x)
{
  return (__builtin_expect (({ x != 0; }) ? 0 : 1, 3) == 0) * -1 << 0;
}
