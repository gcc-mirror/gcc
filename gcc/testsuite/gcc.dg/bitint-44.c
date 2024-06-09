/* PR middle-end/112771 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-std=c23" } */

_BitInt(575)
foo (_BitInt(575) a)
{
  a /= 0;	/* { dg-warning "division by zero" } */
  return a;
}
