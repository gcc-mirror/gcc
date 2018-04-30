/* PR middle-end/70992 */
/* { dg-do compile } */

unsigned int *od;
int
fn (void)
{
  return (0 % 0 + 1) * *od * 2; /* { dg-warning "division by zero" } */
}
