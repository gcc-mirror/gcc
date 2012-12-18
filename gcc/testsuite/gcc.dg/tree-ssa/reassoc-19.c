/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

/* Slightly changed testcase from PR middle-end/40815.  */
void bar(char*, char*, int);
void foo(char* left, char* rite, int element)
{
  while (left <= rite)
  {
    /* This should expand into
       _7 = (sizetype) element_6(D);
       _8 = -_7;
       rite_9 = rite_1 + _8;  */
    rite -= element;
    bar(left, rite, element);
  }
}

/* { dg-final { scan-tree-dump-times "= \\\(sizetype\\\) element" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "= -" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\+ " 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
