/* PR tree-optimization/126103 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-threadfull1-details" } */

int g;

void
h (int a)
{
  void *q;
  if (a)
    q = &&L0;
  else
    q = &&L2;
L2:
  g++;
  goto *q;
L0:
  return;
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 2 "threadfull1" } } */
