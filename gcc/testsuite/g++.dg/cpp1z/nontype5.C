// PR c++/83596
// { dg-do compile { target c++17 } }

struct X { int x; int y; };
template <int X::* mp> int get(X& x) { return x.*mp; }
constexpr int X::* getMP() { return &X::y; }
constexpr int X::* mptr = getMP();
int test() {
    X x{1, 2};
    return get<mptr>(x);
}
