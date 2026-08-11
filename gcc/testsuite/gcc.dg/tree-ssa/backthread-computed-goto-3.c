/* PR tree-optimization/126103 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ethread-details" } */

/* Like backthread-computed-goto-1.c, but check that the early
   threader, which runs without ranger resolution, already threads
   both predecessors of the computed goto.  */

int
f (int a)
{
  void *p;
  if (a)
    p = &&L0;
  else
    p = &&L1;
  goto *p;
L0:
  return 1;
L1:
  return 0;
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 2 "ethread" } } */
