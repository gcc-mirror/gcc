// { dg-do compile }
// { dg-options "-O2 -fdump-tree-optimized"  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>
bool f(const std::vector<bool>& v, std::size_t x) {
	    return v[x];
}
// All references to src should be optimized out, so there should be no name for it
// { dg-final { scan-tree-dump-not "if \\("  optimized } }
