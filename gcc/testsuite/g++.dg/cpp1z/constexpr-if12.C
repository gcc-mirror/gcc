// PR c++/80562
// { dg-options -std=c++17 }

struct T {
  constexpr auto foo() { return false; }
};

template <class MustBeTemplate>
constexpr auto bf(T t) {
    if constexpr(t.foo()) {
        return false;
    }
    return true;
}
