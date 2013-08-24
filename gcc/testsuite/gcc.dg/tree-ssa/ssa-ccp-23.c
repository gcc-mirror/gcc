/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

/* Make sure we propagate through POINTER_PLUS_EXPRs.  */

struct A {
  int i[2];
} a;

int foo (void)
{
  struct A *p = &a;
  int *q = (int *)p;
  int *x = q + 1;
  return *x;
}

/* { dg-final { scan-tree-dump "MEM\\\[\\\(int \\\*\\\)&a \\\+ 2B\\\]" "ccp1" { target { int16 } } } } */
/* { dg-final { scan-tree-dump "MEM\\\[\\\(int \\\*\\\)&a \\\+ 4B\\\]" "ccp1" { target { int32 } } } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
