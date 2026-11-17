/* PR middle-end/128578 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

void
foo (unsigned int x, unsigned int y)
{
  __builtin_clmulg (x, y);
}
