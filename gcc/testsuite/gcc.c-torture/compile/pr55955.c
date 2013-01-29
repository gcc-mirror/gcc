/* PR tree-optimization/55955 */

int b;

void
foo (int x)
{
  int a;
  for (a = x; a < 2; a++)
    for (b = 0; b < 2; b++)
      *(unsigned short *) 0x100000UL %= 46;
}
