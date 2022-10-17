// PR c++/102338
// { dg-do compile { target c++14 } }

struct S {
    template<typename T>
    static auto f(T&& t) noexcept {
        return true;
    }

    template<typename T, typename... Ts>
    static auto f(T&& t, Ts&& ... ts) noexcept(noexcept(f(ts...))) {
        return f(ts...);
    }

};

int main() {
    S::f(true, 0, 5u);
    return 0;
}
