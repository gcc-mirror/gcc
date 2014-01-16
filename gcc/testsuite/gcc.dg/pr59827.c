/* PR middle-end/59827 */
/* { dg-do compile } */

int
foo (int p[2][]) /* { dg-error "array type has incomplete element type" } */
{
  return p[0][0];
}

void
bar (void)
{
  int p[2][1];
  foo (p); /* { dg-error "type of formal parameter 1 is incomplete" } */
}
