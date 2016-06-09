/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

extern int global;

int bar (int);

void foo (int base)
{
  int i;
  while (global < 10)
    for (i = base; i < 10; i++)
      bar (i);
}

/* { dg-final { scan-tree-dump-times "loop branch heuristics of edge\[^:\]*" 0 "profile_estimate"} } */
