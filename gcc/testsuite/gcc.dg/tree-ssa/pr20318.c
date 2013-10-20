/* { dg-do compile { target { ! keeps_null_pointer_checks } } } */
/* { dg-options "-O2 -fdump-tree-original -fdump-tree-vrp1" } */

extern int* f(int) __attribute__((returns_nonnull));
extern void eliminate ();
void g () {
  if (f (2) == 0)
    eliminate ();
}
void h () {
  int *p = f (2);
  if (p == 0)
    eliminate ();
}

/* { dg-final { scan-tree-dump-times "== 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "Folding predicate\[^\\n\]*to 0" 1 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
