/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=2 -fdump-tree-reassoc1" } */

unsigned int
foo (void)
{
  unsigned int a = 0;
  unsigned int b;

  asm volatile ("" : "=r" (b) : "0" (0));
  a += b;
  asm volatile ("" : "=r" (b) : "0" (0));
  a += b;
  asm volatile ("" : "=r" (b) : "0" (0));
  a += b;
  asm volatile ("" : "=r" (b) : "0" (0));
  a += b;

  return a;
}

/* Verify there are two pairs of __asm__ statements with no
   intervening stmts.  */
/* { dg-final { scan-tree-dump-times "__asm__\[^;\n]*;\n *__asm__" 2 "reassoc1"} } */
