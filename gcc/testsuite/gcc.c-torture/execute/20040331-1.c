/* PR c++/14755 */
extern void abort (void);
extern void exit (int);

int
main (void)
{
  struct { int count: 31; } s = { 0 };
  while (s.count--)
    abort ();
  exit (0);
}
