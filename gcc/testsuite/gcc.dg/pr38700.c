/* PR c/38700 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo ()
{
  __SIZE_TYPE__ s = __builtin_expect ((__SIZE_TYPE__)&&L, 0);
L:
  return 0;
}
