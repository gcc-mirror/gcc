/* PR middle-end/38533 */

#define A asm volatile ("" : "=r" (f) : "0" (0)); e |= f;
#define B A A A A A A A A A A A
#define C B B B B B B B B B B B

int
foo (void)
{
  int e = 0, f;
  C C B B B B B A A A A A A
  return e;
}

int
main (void)
{
  if (foo ())
    __builtin_abort ();
  return 0;
}
