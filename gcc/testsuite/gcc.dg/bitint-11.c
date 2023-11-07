/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -std=c23 -pedantic-errors" } */

int
foo (_BitInt(127) x, _BitInt(127) y)
{
  return x < y;
}
