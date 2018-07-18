/* { dg-do compile } */
/* { dg-options "-std=c++14 -O3 -fdump-tree-dse1-details" } */

#include <new>
#include <cstdint>

struct A
{
    std::uint16_t a, b;
};

A* f(char* b) __attribute__((noinline));

A* f(char* b) {
    auto a = new(b) A{};
    a->a = 1;
    a->b = 2;
    return a;
}

int main() {
    char b[sizeof(A)] alignas(A);
    f(b);
}


/* { dg-final { scan-tree-dump "Deleted dead store: " "dse1" } } */

