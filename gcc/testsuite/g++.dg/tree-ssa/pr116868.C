// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" }
#include <vector>
int sumVector() {
    const std::vector<int> vec = {1};
    int sum = 0;
    for (int i = 0; i < vec.size(); i++) {
        sum += vec[i];
    }
    return sum;
}
// { dg-final { scan-tree-dump-not "delete" "optimized" } }
