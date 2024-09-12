// PR c++/105838
// { dg-additional-options -fdump-tree-gimple }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

// Check that there's only one allocator (temporary) variable.
// Currently the dump doesn't print the allocator template arg in this context.
// { dg-final { scan-tree-dump-times "struct allocator D" 1 "gimple" } }

// In the pre-C++11 ABI we get two allocator variables.
#undef _GLIBCXX_USE_CXX11_ABI
#define _GLIBCXX_USE_CXX11_ABI 1

#include <string>

// When the library is not dual-ABI and defaults to old just compile
// an empty TU.  NB: We test _GLIBCXX_USE_CXX11_ABI again because the
// #include above might have undef'd _GLIBCXX_USE_CXX11_ABI.
#if _GLIBCXX_USE_CXX11_ABI

void f (const char *p)
{
  std::string lst[] = { p, p, p, p };
}

#endif
