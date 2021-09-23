// PR c++/101040
// { dg-do compile { target c++11 } }

// This class has to be empty.
struct empty
{};

// This class has to be empty.
struct single
{
    // This member has to be no_unique_address.
    [[no_unique_address]] empty obj;
};

// This class has to be empty and derived from single.
struct derived : single
{
    // This constructor has to be constexpr and take a forwarding reference.
    template <typename Arg>
    constexpr derived(Arg&& arg) : single{arg}
    {}
};

auto obj = derived{empty{}};
