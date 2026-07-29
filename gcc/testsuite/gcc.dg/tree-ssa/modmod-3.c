/* { dg-do compile } */
/* { dg-options "-O2 -ftrapv -fdump-tree-optimized" } */

/* Interpreting -15 as signed proves that three divides it.  The inner
   remainder cannot overflow, so trapping arithmetic does not block the
   fold.  */
int f (int x) { return (x % -15) % 3; }

/* { dg-final { scan-tree-dump-not " % 15;" "optimized" } } */
/* { dg-final { scan-tree-dump-times " % 3;" 1 "optimized" } } */
