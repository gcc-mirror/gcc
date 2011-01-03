/* PR tree-optimization/47155 */

unsigned int a;
static signed char b = -127;
int c = 1;

int
main (void)
{
  a = b <= (unsigned char) (-6 * c);
  if (!a)
    __builtin_abort ();
  return 0;
}
