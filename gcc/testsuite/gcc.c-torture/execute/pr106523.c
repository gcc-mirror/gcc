/* PR tree-optimization/106523 */

__attribute__((noipa)) unsigned char
f7 (unsigned char x, unsigned int y)
{
  unsigned int t = x;
  return (t << y) | (t >> ((-y) & 7));
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || __SIZEOF_INT__ != 4)
    return 0;

  volatile unsigned char x = 152;
  volatile unsigned int y = 19;
  if (f7 (x, y) != 4)
    __builtin_abort ();

  return 0;
}
