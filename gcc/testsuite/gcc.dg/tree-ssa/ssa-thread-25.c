/* PR tree-optimization/126906 */
/* { dg-do compile } */
/* { dg-options "-O2 --param=dom-jump-threading=0 -fdump-tree-ethread-details" } */
/* { dg-require-effective-target label_values } */

/* The computed goto lives in the same block as the address-taken
   label lab2, so the goto's untaken abnormal edge to lab2 points
   back into the block itself.  Threading the resolved paths must
   copy that block.  */

void *sink;

void
foo (int b)
{
  void *x;
  sink = &&lab2;
  if (b)
    x = &&lab;
  else
    x = &&done;
lab2:
  goto *x;
lab:
  sink = 0;
done:
  return;
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 2 "ethread" } } */
/* { dg-final { scan-tree-dump-not "Cancelling" "ethread" } } */
