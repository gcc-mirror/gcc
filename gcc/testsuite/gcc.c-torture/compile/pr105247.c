/* PR target/105247 */

int a;

void
foo (void)
{
  int y = -8;
  a = 1 << y;
}
