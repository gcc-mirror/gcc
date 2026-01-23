// { dg-do compile { target c++17 } }
// { dg-options "-O3 -fdump-tree-optimized" }

#include <algorithm>

bool result;

void func (unsigned long long var_0) {
    result = (std::max ((unsigned long long) 0, (unsigned long long) var_0)) | ( var_0 ?  1 : 0);
}

void func2 (unsigned long long var_0) {
    result = !((std::max ((unsigned long long) 0, (unsigned long long) var_0)) | ( var_0 ?  1 : 0));
}

/* { dg-final { scan-tree-dump-times " \\\| " 0 "optimized" } } */