// PR c++/71117
// { dg-do compile { target c++14 } }

template <class T> T&& declval() noexcept;
template <class, class>
constexpr bool is_same = false;
template <class T>
constexpr bool is_same<T, T> = true;

template <class F>
struct indirected : F {
    indirected(F f) : F(f) {}
    template <class I>
    auto operator()(I i) -> decltype(declval<F&>()(*i)) {
        return static_cast<F&>(*this)(*i);
    }
};

int main() {
    auto f = [](auto rng) {
        static_assert(is_same<decltype(rng), int>, "");
        return 42;
    };
    indirected<decltype(f)> i(f);
    static_assert(is_same<decltype(i(declval<int*>())), int>, "");
}
