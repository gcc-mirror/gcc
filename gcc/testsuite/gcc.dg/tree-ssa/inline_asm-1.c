/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-alias-vops" } */
/* Test to make sure that inline-asm causes a VDEF and that we call test_function twice. */

char test_function(void ) __attribute__((__pure__));
char f(char *a)
{
  char b = test_function();
  asm("":"=m"(*a):"r"(b));
  b = test_function();
  return b;
}

/* test_function should be called twice as the inline-asm changes memory. */
/* { dg-final { scan-tree-dump-times "test_function" 2 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

/* There should a VDEF for the inline-asm.  */
/* { dg-final { scan-tree-dump-times "VDEF" 1 "alias"} } */
/* { dg-final { cleanup-tree-dump "alias" } } */
