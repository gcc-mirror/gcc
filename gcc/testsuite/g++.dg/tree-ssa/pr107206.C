// { dg-do compile }
// { dg-require-effective-target c++17 }
// { dg-options "-O -Wuninitialized" }

#include <optional>
struct X {
    X() = default;
    X(X const& r) : i(r.i) {}
    int i;
};
struct Y {
    Y() : x() {}
    X x;
    std::optional<int> o;
};
struct Z {
    Y y;
    explicit Z(Y y) : y(y) {}
};
void f(Y const&);
void test() {
    Y const y;
    Z z(y);
    z.y.o = 1;
    auto const w = z;
    f(w.y);
}
