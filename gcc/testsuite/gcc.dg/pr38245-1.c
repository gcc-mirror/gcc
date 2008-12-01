/* PR rtl-optimization/38245 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static inline int
f1 (int si1, int si2)
{
  return si2 == 0 ? si1 : si1 / si2;
}

static inline unsigned long long
f2 (unsigned long long ui1, unsigned long long ui2)
{
  return ui1 % ui2;
}

unsigned char g;
volatile unsigned int h;

void
f3 (void)
{
  if (!((signed char) f1 (0, f2 (g, 2123)) - 1))
    h;
}

int
main (void)
{
  f3 ();
  return 0;
}

/* { dg-final { scan-tree-dump-not "% 2123" "optimized" } } */
/* { dg-final { scan-tree-dump-not "0 / " "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
