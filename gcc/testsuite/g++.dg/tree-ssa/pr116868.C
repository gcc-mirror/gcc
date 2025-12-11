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
// For C++26 delete appears in ~bad_array() and ~bad_array_new_length() dtors.
// { dg-final { scan-tree-dump-not "delete" "optimized" { target c++23_down } } }
