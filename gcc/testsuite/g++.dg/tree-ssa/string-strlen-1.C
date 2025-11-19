// { dg-do compile } */
// { dg-additional-options "-fdump-tree-optimized -O2" }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
// PR tree-optimization/122754

#include <string>

int g(void)
{
  std::string a="a";
  return __builtin_strlen(a.c_str());
}

// This should be optimized to just `return 1` without any
// references to struct string or any other variables.

// { dg-final { scan-tree-dump-not "strlen " "optimized" } }
// { dg-final { scan-tree-dump-not "struct string" "optimized" } }
// { dg-final { scan-tree-dump "return 1" "optimized" } }

