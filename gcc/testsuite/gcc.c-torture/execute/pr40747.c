/* PR middle-end/40747 */

extern void abort (void);

int
foo (int i)
{
  return (i < 4 && i >= 0) ? i : 4;
}

int
main ()
{
  if (foo (-1) != 4) abort ();
  if (foo (0) != 0) abort ();
  if (foo (1) != 1) abort ();
  if (foo (2) != 2) abort ();
  if (foo (3) != 3) abort ();
  if (foo (4) != 4) abort ();
  if (foo (5) != 4) abort ();
  return 0;
}
