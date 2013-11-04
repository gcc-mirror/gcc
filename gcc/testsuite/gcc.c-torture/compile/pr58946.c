/* PR tree-optimization/58946 */

int
foo (unsigned int c)
{
  unsigned int d, e, f;
  if ((int) c < 0)
    d = 0;
  else
    d = c;
  if (d == 0)
    e = __INT_MAX__ + 1U;
  else
    e = d;
  if ((int) e < 0)
    f = 0;
  else
    f = e;
  return f;
}
