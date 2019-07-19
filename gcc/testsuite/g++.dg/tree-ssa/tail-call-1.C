// PR c++/82081
// { dg-do compile { target c++11 } }
// { dg-additional-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-not "tail call" "optimized" } }

int g(int) ;

int f() noexcept {
    int i = 42, j = 43;
    return g(i+j);
}
