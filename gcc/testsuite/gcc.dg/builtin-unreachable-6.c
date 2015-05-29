/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fab1" } */

void
foo (int b, int c)
{
  void *x = &&lab;
  if (b)
    {
lab:
      __builtin_unreachable ();
    }
lab2:
  if (c)
    x = &&lab2;
  goto *x;
}

/* { dg-final { scan-tree-dump-times "lab:" 1 "fab1" } } */
/* { dg-final { scan-tree-dump-times "__builtin_unreachable" 1 "fab1" } } */
