// { dg-do compile }
// { dg-options "-O3 -fdump-tree-vrp2"  }

#include <vector>

void shrink(std::vector<int>& v, unsigned n) {
    if (v.size() < n)
      __builtin_unreachable();
    v.resize(v.size() - n);
}

// Verify that std::vector<T>::_M_default_append() has been removed by vrp2.
// { dg-final { scan-tree-dump-not "_M_default_append"  vrp2 } }
