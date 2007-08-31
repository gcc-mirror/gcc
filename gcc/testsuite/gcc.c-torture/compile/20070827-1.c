/* PR rtl-optimization/33148 */

int
foo (unsigned int *p, int *q, unsigned int w, unsigned int b)
{
  unsigned int i;
  int mask;

  if (q[0] < q[1])
    mask = 0xff;
  else
    mask = 0;

  for (i = 0; 8 * i < w; i++)
    {
      b ^= mask;
      *p++ = b;
    }
  return 0;
}
