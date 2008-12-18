/* PR middle-end/38533 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

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

/* Verify that reassoc hasn't increased register pressure too much
   by moving all bitwise ors after the last __asm__.  There should
   be exactly 2 (first) __asm__ stmts with no intervening stmts,
   all others should have some bitwise or in between.  */
/* { dg-final { scan-tree-dump-times "__asm__\[^;\n]*;\n *__asm__" 1 "reassoc1"} } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
