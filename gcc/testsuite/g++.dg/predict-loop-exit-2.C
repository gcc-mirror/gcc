/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate -fdisable-tree-ethread" } */

int g;
int foo();
void test() {
  while (foo() || g < 10)
    g++;
  return;
}

/* { dg-final { scan-tree-dump-times "extra loop exit heuristics of edge\[^:\]*:" 1 "profile_estimate"} } */
/* { dg-final { scan-tree-dump-times "loop exit heuristics of edge\[^:\]*:" 2 "profile_estimate"} } */
