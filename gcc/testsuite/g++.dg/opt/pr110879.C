// { dg-do compile { target c++11 } }
// { dg-options "-O3 -fdump-tree-optimized" }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>

std::vector<int> f(std::size_t n) {
  std::vector<int> res;
  for (std::size_t i = 0; i < n; ++i) {
    res.push_back(i);
  }
  return res;
}

// Reads of _M_finish should be optimized out.
// This regex matches all reads from res variable except for _M_end_of_storage field.
// { dg-final { scan-tree-dump-not "=\\s*\\S*res_(?!\\S*_M_end_of_storage;)" "optimized" } }
