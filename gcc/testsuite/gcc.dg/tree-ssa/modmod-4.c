/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow -fdump-tree-optimized" } */

int f1 (int x) { return (x % -1) % 1; }
int f2 (int x) { return (x % -15) % 3; }

/* Keep the possible INT_MIN % -1 diagnostic in f1.  The safe f2 pair still
   folds.  */
/* { dg-final { scan-tree-dump-times "__ubsan_handle_divrem_overflow" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " % 15;" "optimized" } } */
/* { dg-final { scan-tree-dump-times " % 3;" 1 "optimized" } } */
