/* PR tree-optimization/126906 */
/* { dg-do compile } */
/* { dg-options "-O2 --param=dom-jump-threading=0 -fdump-tree-threadfull1-details" } */
/* { dg-require-effective-target label_values } */

/* The path to the computed goto resolves it to lab.  It is the
   address-taken label lab2 that is targeted by the goto's own untaken
   abnormal edge back into the region being copied.  */

void
foo (int b, int c)
{
  void *x = &&lab;
  if (b)
    {
lab:
      return;
    }
lab2:
  if (c)
    x = &&lab2;
  goto *x;
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 1 "threadfull1" } } */
/* { dg-final { scan-tree-dump-not "Cancelling" "threadfull1" } } */
