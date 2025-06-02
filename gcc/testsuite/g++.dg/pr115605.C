// { dg-do compile { target c++20 } }

#include <array>

int foo() {
    int const tuple_size = 5;
    std::array<int, 3> array {1, 2, 3};
    auto [a, b, c] = array;
    return c;
}
