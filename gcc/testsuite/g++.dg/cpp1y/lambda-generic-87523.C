// PR c++/87523
// { dg-do compile { target c++14 } }

template <typename T, T v>
struct my_integer_constant {
    constexpr my_integer_constant() {}
    constexpr operator T() const { return v; }
    constexpr T operator()() const { return v; }
};

template <typename T, T... u>
struct constant_call {
    template <typename Callback>
    static void call(T v, Callback f) {
        char dummy[sizeof...(u)] = { ( (v == u) ? (f(my_integer_constant<T, u>{}), static_cast<char>(0)) : static_cast<char>(0))... };
        (void)dummy;
    }
};

void f(bool reverse_in, bool other_bool_in) {
    auto helper = [&] (auto reverse_t) {
        bool constexpr reverse_v = reverse_t;
        (void)reverse_v;
        constant_call<bool, true, false>::call(other_bool_in,
                                               [&] (auto newb) {
                                                   bool reverse_v_dyn = reverse_v;
                                               });
    };
    constant_call<bool, true, false>::call(reverse_in, [&] (auto reverse_t) {
            helper(reverse_t);
        });
}
