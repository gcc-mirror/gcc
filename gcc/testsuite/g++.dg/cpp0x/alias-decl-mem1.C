// PR c++/103968
// { dg-do compile { target c++11 } }

template <typename Opt, Opt const& options>
struct trait
{
    template <typename T>
    struct NonInstantiated{};
};

struct Options {};

template <typename T>
struct Widget
{
    static constexpr auto c_options = Options{};
    using Trait = trait<decltype(c_options), c_options>;
};

Widget<int>::Trait b{}; // Crashes GCC > 10.3
