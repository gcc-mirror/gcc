// { dg-do compile { target c++20 } }
template<class> constexpr bool is_int = false;
template<> constexpr bool is_int<int> = true;

template <class T>
concept Int = is_int<T>;

int main() {
    auto x = []<Int T>(T t) { return 42; };
    auto y = x(42);
    auto z = x(""); // { dg-error "no match" }
    return z;
}
