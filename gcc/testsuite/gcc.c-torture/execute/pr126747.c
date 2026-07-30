/* PR rtl-optimization/126747 */
/* Both overflow tests read one condition-code value that late-combine left
   live across the branches.  If-conversion emitted a fresh comparison at the
   end of the test block and destroyed it.  */

volatile int c[2];

__attribute__((noipa)) int
foo (unsigned x, unsigned y)
{
  unsigned r = x * y;
  int t = 0;
  if (c[0]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  if (c[1]) { int u = 0; if (x != 0) u = (r / x != y); t += u; }
  return t;
}

int
main (void)
{
  c[0] = 1;
  c[1] = 1;
  if (foo (1, 1) != 0)
    __builtin_abort ();
  return 0;
}
