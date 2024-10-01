/* PR middle-end/116898 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O -finstrument-functions -fnon-call-exceptions" } */

_BitInt(127) a;
_BitInt(511) b;

void
foo (_BitInt(31) c)
{
  do
    {
      c %= b;
again:
    }
  while (c);
  a /= 0;		/* { dg-warning "division by zero" } */
  c -= a;
  goto again;
}
