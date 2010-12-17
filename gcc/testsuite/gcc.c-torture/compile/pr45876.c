/* PR middle-end/45876 */

unsigned
foo (unsigned x)
{
  short i = 0;
  i = ((short) (((((unsigned) i) >> 1) & 16383) + x)) & 16383;
  return i;
}
