/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-tree-dominator-opts -fno-tree-vrp" } */
/* { dg-require-effective-target label_values } */

void foo (int b, int c)
{
  __label__ lab;
  __label__ lab2;
  static void *x[2] = {&&lab, &&lab2};
  if (b == c)
    {
lab:
      __builtin_unreachable ();
    }
lab2:
  goto *x[c!=0];
}

/* Fab should still able to remove the conditional but leave the bb there. */

/* { dg-final { scan-tree-dump-times "lab:" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */

