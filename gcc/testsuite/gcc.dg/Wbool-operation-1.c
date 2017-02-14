/* PR c/77490 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

int
fn (_Bool b)
{
  int r = 0;

  r += b++; /* { dg-warning "increment of a boolean expression" } */
  r += ++b; /* { dg-warning "increment of a boolean expression" } */
  r += b--; /* { dg-warning "decrement of a boolean expression" } */
  r += --b; /* { dg-warning "decrement of a boolean expression" } */

  return r;
}
