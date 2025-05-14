// { dg-do compile }
// { dg-additional-options "-O2 -fdump-tree-optimized" }
#include<string>
int sain() {
  const std::string remove_me("remove_me");
  return 0;
}
// { dg-final { scan-tree-dump-not "remove_me" "optimized" } }
