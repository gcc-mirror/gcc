/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdump-tree-dce1" } */

int
foo (int *p)
{
  int a = *p;
  int b = p != 0;

  *p = b;

  if (b)
    return a;
  else
    return 0;
}

/* Target with fno-delete-null-pointer-checks should not fold checks */
/* { dg-final { scan-tree-dump "Folding predicate " "vrp1" { target { ! keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-times "Folding predicate " 0 "vrp1" { target {   keeps_null_pointer_checks } } } } */
/* { dg-final { scan-tree-dump-not "b_. =" "dce1" { target { ! avr-*-* } } } } */
/* { dg-final { scan-tree-dump "b_. =" "dce1" { target { avr-*-* } } } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
/* { dg-final { cleanup-tree-dump "dce1" } } */
