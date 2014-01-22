/* PR c/59891 */

unsigned int a;

int
main ()
{
  return (0 ? a : 0) ? : 0 % 0; /* { dg-warning "division by zero" } */
}
