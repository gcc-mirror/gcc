/* PR c/93949 */

void
foo (void)
{
  register const double d[3] = { 0., 1., 2. };
}
