/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ivopts" } */

void bar (int, int, int);
void foo (int left, int rite, int element)
{
  while (left <= rite)
    {
      rite -= element;
      bar (left, rite, element);
      left += element;
    }
}

/* { dg-final { scan-tree-dump-times "rite_\[0-9\]* = rite_\[0-9\]* - element" 1 "ivopts"} } */
/* { dg-final { scan-tree-dump-times "left_\[0-9\]* = left_\[0-9\]* \\+ element|left_\[0-9\]* = element_\[0-9\]*\\(D\\) \\+ left" 1 "ivopts"} } */
