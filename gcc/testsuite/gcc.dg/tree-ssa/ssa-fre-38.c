/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

struct S { int i; int j; };

struct U
{
  struct S a[10];
} u;

int foo (int n, int i, int j)
{
  u.a[n].i = i;
  u.a[n].j = j;
  return u.a[n].i;
}

/* We should remove the redundant load.  */

/* { dg-final { scan-tree-dump-not "= u.a\\\[n_2\\(D\\)\\\].i" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
