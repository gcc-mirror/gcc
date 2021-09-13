// PR c++/90799
// { dg-do compile { target c++17 } }

template<class T>
void foo() noexcept(T::value);

struct S {
    static constexpr const bool value = true;

    template<class T>
    void bar() noexcept(T::value);
};

template<class... Args, bool is_noexcept>
constexpr bool is_noexcept_function(void(Args...) noexcept(is_noexcept)) noexcept {
    return is_noexcept;
}

template<class... Args, bool is_noexcept>
constexpr bool is_noexcept_member_function(void(S::*)(Args...) noexcept(is_noexcept)) noexcept {
    return is_noexcept;
}

static_assert(is_noexcept_function(foo<S>));
