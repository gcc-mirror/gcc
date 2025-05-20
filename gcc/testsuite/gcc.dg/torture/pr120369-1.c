/* { dg-do compile } */
/* PR middle-end/120369 */

/* Make sure cabs without a lhs does not cause an ICE. */
void f()
{
  double _Complex z = 1.0;
  __builtin_cabs(z);
}
