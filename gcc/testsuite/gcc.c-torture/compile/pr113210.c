/* PR tree-optimization/113210 */

unsigned char a, c;
unsigned short b;

void
foo (void)
{
  c = a + 255;
  b = c;
  while (++b > 256)
    ;
}
