/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate -std=c++11" } */

int a, b, c;

void
bar()
{
  if (a == 123)
    [[likely]] c = 5;
  else
    c = 5;
}

/* { dg-final { scan-tree-dump "first match heuristics: 90.00%" "profile_estimate"} } */
/* { dg-final { scan-tree-dump "hot label heuristics of edge .*->.*: 90.00%" "profile_estimate"} } */
