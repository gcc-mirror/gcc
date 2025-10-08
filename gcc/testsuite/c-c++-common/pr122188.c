/* PR c/122188 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (const unsigned x, int y)
{
  return __builtin_ctzg (x ? x : 4081577U, y);
}

int
bar (const unsigned x, int y)
{
  return __builtin_clzg (x ? x : 4081577U, y);
}
