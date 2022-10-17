/* { dg-do compile } */
/* { dg-options "-fharden-conditional-branches -fdump-tree-hardcbr -ffat-lto-objects" } */

extern int f1 (void);
extern int f2 (void);


int
f (int i, int j)
{
  return (i < j) ? f1 () : f2 ();
}

/* { dg-final { scan-tree-dump-times "Splitting edge" 2 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times "Adding reversed compare" 2 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times "__builtin_trap" 2 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times "if \[(\]i_\[0-9\]*\[(\]D\[)\] < j_\[0-9\]*\[(\]D\[)\]\[)\]" 1 "hardcbr" } } */
/* { dg-final { scan-tree-dump-times "if \[(\]_\[0-9\]* >= _\[0-9\]*\[)\]" 2 "hardcbr" } } */
