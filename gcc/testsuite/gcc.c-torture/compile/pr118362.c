/* PR target/118362 */

int a, b, c[18];

void
foo (void)
{
  for (int i = 0; i < 8; i++)
    if (b)
      {
	c[i * 2 + 1] = a;
	c[i * 2 + 2] = 200;
      }
    else
      {
	c[i * 2 + 1] = 1000000;
	c[i * 2 + 2] = 200;
      }
}
