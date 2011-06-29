// PR c++/49520
// { dg-options -std=c++0x }

namespace x { void foo(); }

template<typename T>
struct traits
{
    static constexpr bool f() { return true; }

    static constexpr bool g()
    {
        using x::foo;
        return f() && noexcept(foo());
    }
};

template struct traits<int>;
