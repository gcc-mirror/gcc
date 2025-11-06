/* Disable warnings about __sync_nand_and_fetch. */
/* { dg-options "-w" } */
/* PR tree-optimization/122588 */

int i;
char c;

static inline __attribute__((__always_inline__))
void foo0 (int a)
{
l5:
  __sync_nand_and_fetch (&i, 0);
  int x = __builtin_memcmp_eq (&a, 0, 4);
  if (__builtin_iseqsig (x, 0.))
    goto l5;
  if (a)
    __builtin_unreachable ();
  c = a;
}

int
main ()
{
  foo0 (1);
}
