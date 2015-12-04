/* PR tree-optimization/68680 */
/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-strong" } */

int foo (char *);

int
bar (unsigned long x)
{
  char a[x];
  return foo (a);
}

/* Verify that this function is stack protected.  */
/* { dg-final { scan-assembler "stack_chk_fail" } } */
