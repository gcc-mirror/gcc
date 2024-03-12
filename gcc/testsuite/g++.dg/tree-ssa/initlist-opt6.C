// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-not {static const struct S} "gimple" } }

// Test that mutable prevents putting this init-list array in rodata.

#include <initializer_list>

struct S {
    constexpr S(int i) : i(i) {}
    mutable int i;
};

void f(std::initializer_list<S>);

int main() {
    f({1,2,3});
}
