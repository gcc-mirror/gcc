/* PR middle-end/117458 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -O2" } */

typedef _BitInt(33) B __attribute__((may_alias));

_BitInt(33)
foo (_Complex float x)
{
  return *(B *)&x;
}
