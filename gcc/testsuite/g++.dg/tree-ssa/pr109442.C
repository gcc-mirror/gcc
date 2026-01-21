// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-skip-if "required hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>
#define T int
T vat1(std::vector<T> v1) {
    auto v = v1;
    return 10;
}
// This should compile to empty function; check that no size of
// vector is determined and there is no allocation
// { dg-final { scan-tree-dump-not "_M_start" "optimized" } }
// For C++26 delete appears in ~bad_array() and ~bad_array_new_length() dtors.
// { dg-final { scan-tree-dump-not "delete" "optimized" { target c++23_down } } }
