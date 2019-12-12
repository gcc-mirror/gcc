/* PR middle-end/92063 */

int
foo (int a, int b, int *c, short *d)
{
  return (c[0] ? b : 0) == 'y' && ((a ? d[0] : c[0]) ? b : 0) == 'c';
}
