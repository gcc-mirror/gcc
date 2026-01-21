// PR c++/113835
// { dg-timeout-factor 0.05 }
// { dg-do compile { target c++20_only } }
// { dg-skip-if "required hosted libstdc++ for vector" { ! hostedlib } }

#include <vector>
const std::size_t N = 1'000'000;
std::vector<int> x(N);
int main() {}
