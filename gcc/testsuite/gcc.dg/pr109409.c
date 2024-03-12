/* PR c/109409 */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void
foo (int n)
{
  const char c[n] = "1";	/* { dg-error "variable-sized object may not be initialized except with an empty initializer" } */
  __builtin_printf (c);
}
