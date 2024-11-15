// { dg-do compile { target c++11 } }
// { dg-options "-O1 -fdump-tree-optimized" }
#include<vector>

void f(int);

void use_idx_const_size_reserve() {
    std::vector<int> v;
    v.reserve(100000);
    auto s = v.size();
    for (std::vector<int>::size_type i = 0; i < s; i++)
        f(v[i]);
}
// { dg-final { scan-tree-dump-not "delete" "optimized" } }
