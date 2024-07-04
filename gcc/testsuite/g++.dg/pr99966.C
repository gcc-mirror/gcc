// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-options "-O2 -fdump-tree-vrp1" }

// Test we can remove a range bound after the assert.

#include <cassert>
#include <cstdint>
#include <cstddef>
#include <vector>

uint64_t f(std::vector<uint64_t>& data, size_t start, size_t end){
    assert(start < end && start < data.size() && end <= data.size());


    uint64_t total = 0;
    for (size_t i = start; i < end; i++) {
        total += data.at(i);
    }
    return total;
}

/* { dg-final { scan-tree-dump-not "throw" "vrp1" { xfail hppa*64*-*-* } } } */
