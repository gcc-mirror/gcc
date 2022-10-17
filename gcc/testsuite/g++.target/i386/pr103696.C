// PR c++/103696
// { dg-options "-O2 -std=c++14 -fdump-tree-optimized" }

int global_var;

void fn() {
}

#pragma GCC optimize("finite-math-only")
#pragma GCC target("sse3")

void fn2() {
}

void fn3() {
}

int solve() {
    auto nested = []() {
        return global_var;
    };
    return nested();
}

/* { dg-final { scan-tree-dump-not "lambda" "optimized" } } */
