/* PR middle-end/37931 */

extern void abort (void);

int
foo (int a, unsigned int b)
{
  return (a | 1) & (b | 1);
}

int
main (void)
{
  if (foo (6, 0xc6) != 7)
    abort ();
  if (foo (0x80, 0xc1) != 0x81)
    abort ();
  if (foo (4, 4) != 5)
    abort ();
  if (foo (5, 4) != 5)
    abort ();
  return 0;
}
