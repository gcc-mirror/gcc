// P2564R3
// { dg-do compile { target c++20 } }
// Cribbed from clang's cxx2b-consteval-propagate.cpp.

consteval int id(int i) { return i; }

template <typename T>
constexpr int f(T t);

auto a1 = &f<char>;
auto b1 = &f<int>;

template <typename T>
constexpr int f(T t) {
    return id(0);
}

template <typename T>
constexpr int f2(T);

auto a2 = &f2<char>; // { dg-error "taking address" }
auto b2 = &f2<int>; // { dg-error "taking address" }

template <typename T>
constexpr int f2(T t) {
    return id(t);
}
