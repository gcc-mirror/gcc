// { dg-do compile { target c++1y } }

#include <chrono>

using namespace std::literals::chrono_literals;

auto blooper = operator"" min(45.0L);
