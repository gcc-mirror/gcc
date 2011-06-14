/* PR middle-end/45262 */

extern void abort (void);

int
foo (unsigned int x)
{
  return ((int) x < 0) || ((int) (-x) < 0);
}

int
bar (unsigned int x)
{
  return x >> 31 || (-x) >> 31;
}

int
main (void)
{
  if (foo (1) != 1)
    abort ();
  if (foo (0) != 0)
    abort ();
  if (foo (-1) != 1)
    abort ();
  if (bar (1) != 1)
    abort ();
  if (bar (0) != 0)
    abort ();
  if (bar (-1) != 1)
    abort ();
  return 0;
}
