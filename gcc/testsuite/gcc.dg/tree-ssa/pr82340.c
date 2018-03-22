/* PR c/82340 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ssa" } */
/* { dg-final { scan-tree-dump "D.\[0-9]*\\\[0\\\] ={v} 77;" "ssa" } } */

int
foo (void)
{
  int i;
  volatile char *p = (volatile char[1]) { 77 };
  for (i = 1; i < 10; i++)
    *p = 4;
  return *p;
}
