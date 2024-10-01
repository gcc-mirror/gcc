/* PR middle-end/116899 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O2" } */

float f;
_BitInt(255) b;

void
foo (signed char c)
{
  for (;;)
    {
      c %= (unsigned _BitInt(512)) 0;	/* { dg-warning "division by zero" } */
      f /= b >= c;
    }
}
