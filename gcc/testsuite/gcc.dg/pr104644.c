/* PR tree-optimization/104644 */
/* { dg-do compile } */
/* { dg-options "-Wno-overflow" } */

int
foo (void)
{
  return __builtin_bswap16 (1.31072e+5f) != (signed char) 1.31072e+5f;
}
