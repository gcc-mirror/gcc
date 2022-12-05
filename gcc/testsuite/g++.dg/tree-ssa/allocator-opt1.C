// PR c++/105838
// { dg-additional-options -fdump-tree-gimple }

// Check that there's only one allocator (temporary) variable.
// Currently the dump doesn't print the allocator template arg in this context.
// { dg-final { scan-tree-dump-times "struct allocator D" 1 "gimple" } }

#include <string>
void f (const char *p)
{
  std::string lst[] = { p, p, p, p };
}
