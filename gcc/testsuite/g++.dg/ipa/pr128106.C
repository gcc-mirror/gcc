/* { dg-do compile { target c++17 } } */
/* { dg-options "-O2" } */

#include <array>
#include <string_view>
#include <tuple>

struct F { std::string_view name = "x"; };
constexpr std::array<F, 256> fields{};

[[gnu::noinline]] void emit(std::string_view name) {
    if (name != "x") __builtin_puts("miscompiled");
}

int main() {
    long value{};
    std::apply([&](auto... f) { ((emit(f.name), value += value), ...); }, fields);
    return value;
}
