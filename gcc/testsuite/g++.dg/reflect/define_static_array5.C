// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>
#include <ranges>

static_assert(
    std::ranges::equal(
        (std::vector {1, 2} | std::views::transform([](auto z) { return std::pair(z, z); })),
        std::vector {std::pair{1, 1}, std::pair{2, 2}}));
static_assert(
    std::ranges::equal(
        std::define_static_array(
            (std::vector {1, 2} | std::views::transform([](auto z) { return std::pair(z, z); }))),
        std::vector {std::pair{1, 1}, std::pair{2, 2}}));
static_assert(
    std::ranges::equal(
        std::define_static_array(
            (std::vector {1, 2} | std::views::transform([](auto z) { return z; }))),
        std::vector {1, 2}));
