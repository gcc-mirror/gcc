/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdelete-null-pointer-checks" } */

struct A
{
  int a;
  int b;
};

int
foo (struct A *p, struct A *q)
{
  int x = p->a;
  if (p == q)
    return q->a;

  /* We should fold this to 'if (1)' but the assertion for 'p == q'
     was overwriting the assertion 'p != 0' from the first dereference
     of 'p'.  */
  if (p)
    return x + p->b;
}
/* Target disabling -fdelete-null-pointer-checks should not fold check */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 1 "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Folding predicate p_.*to 1" 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */
