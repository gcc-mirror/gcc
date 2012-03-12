/* PR tree-optimization/52533 */

int
foo (unsigned char x)
{
  if (x <= 9)
    return '0' + x;
  else if (x <= 15)
    return 'a' + (x - 10);
  else
    return 0;
}

void
bar (unsigned char x, unsigned char *y)
{
  y[0] = foo ((unsigned char) (x >> 4));
  y[1] = foo ((unsigned char) (x & 0x0f));
}
