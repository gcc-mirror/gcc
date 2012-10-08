/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

int g;
int foo();
void test() {
  while (foo() || g < 10)
    g++;
  return;
}

/* { dg-final { scan-tree-dump-times "loop exit heuristics:" 2 "profile_estimate"} } */
/* { dg-final { cleanup-tree-dump "profile_estimate" } } */
