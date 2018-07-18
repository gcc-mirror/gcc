/* PR debug/85252 */
/* { dg-do compile } */

void
foo (void)
{
  static char a[0] = "";
  static char b[0] = "b";	/* { dg-warning "initializer-string for array of chars is too long" } */
  static char c[1] = "c";
  static char d[1] = "de";	/* { dg-warning "initializer-string for array of chars is too long" } */
}
