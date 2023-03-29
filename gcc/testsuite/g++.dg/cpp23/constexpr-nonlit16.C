// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++20 } }
// { dg-options "" }

template <typename T>
struct Wrapper {
    constexpr Wrapper() = default;
    constexpr Wrapper(Wrapper const&) = default;
    constexpr Wrapper(T const& t) : t(t) { }

    constexpr T get() const { return t; }
    constexpr bool operator==(Wrapper const&) const = default; // { dg-warning "call to" "" { target c++20_down } }
private:
    T t;
};

struct X {
    X();
    bool operator==(X const&) const;
};

Wrapper<X> x;
