/* PR target/16364 */
union foo {
  long double ld;
} bar;

double
sub (union foo baz)
{
  return baz.ld / 2;
}
