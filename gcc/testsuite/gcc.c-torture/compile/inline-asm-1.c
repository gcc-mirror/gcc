/* PR tree-opt/106087,
   simple_dce_from_worklist would delete the
   inline-asm when it was still being referenced
   by the other ssa name. */

static int t;

int f(void)
{
  int tt, tt1;
  asm("":"=r"(tt), "=r"(tt1));
  t = tt1;
  return tt;
}
