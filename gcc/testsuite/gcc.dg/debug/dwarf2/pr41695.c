/* PR debug/41695 */
/* { dg-do compile } */
/* { dg-options "-gdwarf -O2 -dA -fno-merge-debug-strings" } */

int bar (int);

void
foo (void)
{
  int b = 0;
  b = bar (b);
  b = bar (b);
  b = bar (b);
  b = bar (b);
  bar (b);
}

/* { dg-final { scan-assembler-not "LVL(\[0-9\]+)-\[^1\]\[^\\r\\n\]*Location list begin address\[^\\r\\n\]*\[\\r\\n\]+\[^\\r\\n\]*LVL\\1-1-" } } */
