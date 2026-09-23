/* PR middle-end/126497 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23" } */

typedef unsigned _BitInt (1) U;

U
foo (U a)
{
  U t = a >= 1uwb;
  return t;
}

U
bar (U a)
{
  U t = a == 1uwb;
  return t;
}
