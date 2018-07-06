/* PR c/80525 */
/* { dg-do compile } */
/* { dg-options "-Wlogical-op" } */

int
fn (int a, int b)
{
  if ((a + 1) && (a + 1)) /* { dg-warning "logical .and. of equal expressions" } */
    return a;
  if ((a + 1) || (a + 1)) /* { dg-warning "logical .or. of equal expressions" } */
    return b;
  return -1;
}
