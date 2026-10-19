/* PR rtl-optimization/127725 */
/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=v8 -fno-var-tracking -fcompare-debug --param=max-delay-slot-live-search=100000 --param=max-delay-slot-insn-search=100000" } */

int b;

void
foo (int n, char *c)
{
  for (;;)
    for (int f = 0; f < n; f++)
      b = c[f];
}
