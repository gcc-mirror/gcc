// { dg-options -std=c++1y }
// { dg-do compile }

#include <chrono>

using namespace std::literals::chrono_literals;

auto blooper = operator"" min(45.0L);
