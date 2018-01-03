/* PR tree-optimization/83593 */
/* { dg-options "-O2 -fno-tree-dominator-opts -fnon-call-exceptions -fno-tree-pre -fexceptions -fno-code-hoisting -fno-tree-fre" } */

void
hr (int *ed, signed char *ju)
{
  int kc;
    {
      int xj;
      int *q2 = (*ed == 0) ? &xj : &kc;

      *ju = 0;
      kc = *ju;
    }
}
