/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

/* PR c/24599 */

int
main (void)
{
  if ((_Bool)(__INT_MAX__ + 1)) /* { dg-warning "overflow in expression" } */
    return 1;
  else
    return 0;
}
