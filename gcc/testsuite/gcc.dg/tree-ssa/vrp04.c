/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdump-tree-vrp1" } */

int
foo (int a, int b)
{
  if (a == b)
    /* This should be folded to if (1)  */
    if (a == b)
      return a + b;
}

/* { dg-final { scan-tree-dump-times "if" 1 "vrp1" } } */
