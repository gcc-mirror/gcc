// { dg-do compile { target c++14 } }
// { dg-skip-if "requires hosted libstdc++ for chrono" { ! hostedlib } }

#include <chrono>

using namespace std::literals::chrono_literals;

auto blooper = operator ""min(45.0L);
