/* PR c/80919 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void
fn (void)
{
  int a[0];
  __builtin_printf("%d\n", &a); /* { dg-warning "expects argument of type" } */
  __builtin_printf("%i\n", &a); /* { dg-warning "expects argument of type" } */

  __builtin_printf("%o\n", &a); /* { dg-warning "expects argument of type" } */
  __builtin_printf("%u\n", &a); /* { dg-warning "expects argument of type" } */
  __builtin_printf("%x\n", &a); /* { dg-warning "expects argument of type" } */
  __builtin_printf("%X\n", &a); /* { dg-warning "expects argument of type" } */
}
