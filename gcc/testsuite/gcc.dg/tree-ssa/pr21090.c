/* { dg-do compile } */
/* { dg-options "-O2 -fno-thread-jumps -fdisable-tree-evrp -fdump-tree-vrp1 -fdelete-null-pointer-checks" } */

int g, h;

int
foo (int a)
{
  int *p;

  if (a)
    p = &g;
  else
    p = &h;

  if (p != 0)
    return 1;
  else
    return 0;
}

/* { dg-final { scan-tree-dump-times "Folding predicate.*to 1" 1 "vrp1" { target { ! keeps_null_pointer_checks } } } } */
